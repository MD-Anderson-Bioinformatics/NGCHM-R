# Install a shaidy format NGCHM in a shaidy repository.
# This version assumes no one else is writing to the repository concurrently.
#

shaidy.env <- new.env(parent=emptyenv());

#' Obtain the git hash of an existing file.
#'
#' @param path filename of file to hash
#'
#' @return a string containing the file hash
gitHashObject <- function (path) {
    stopifnot (file.exists (path));
    hash <- system2 ("git", c("hash-object", path), stdout=TRUE);
    stopifnot (is.null (attr (hash, "status")));
    hash
}

#' Get the methods for the repository API called api
#'
#' @param api The name of a repository API
#'
#' @return A list of repository methods
shaidyRepoAPI <- function (api) {
    shaidy.env$repoMethods[[api]]
}

#' Set the methods for the repository API called api
#'
#' @param api The name of a repository API
#' @param methods A list of repository methods
shaidyRegisterRepoAPI <- function (api, methods) {
    shaidy.env$repoMethods[[api]] <- methods;
}

#' Provide a simpler method for accessing repo methods
#'
#' @param repo The repository to obtain the method for
#' @param method The name of the method to obtain
#'
#' @return A function that calls the method with the repository as its first parameter
#'
#' @export
"$.shaidyRepo" <- function (repo, method) {
    method <- substitute (method);
    if (method %in% names(repo)) {
        return (repo[[method]]);
    }
    api <- repo[['accessMethod']];
    while ((length(api) > 0) && (api %in% names(shaidy.env$repoMethods))) {
	mtab <- shaidy.env$repoMethods[[api]];
	if (method %in% names(mtab)) {
	    return (function (...) do.call(mtab[[method]], list(repo,...)));
	}
	api <- mtab[["__super__"]];
    }
    return (NULL);
}

#' Initialize the shaidy subsystem
#'
#' blobPath returns a function (first, ...) that accepts either a shaid or a blob type and optionally
#'         additional file path components and returns a filepath
#'
#' @import jsonlite
#' @import httr
#'

shaidyInit <- function() {
    shaidy.env$repoMethods <- list();
    shaidyRegisterRepoAPI ("__generic__", list (
	# Load a collection from a shaidy repository
	#
	# @param repo The shaidy repository
	# @param collection.uuid A string containing the UUID of the collection to load.
	#
	# @return an ngchmCollection object containing the collection data
	#
	# @import jsonlite
	#
	# @export
	loadCollection = function (repo, collection.uuid="") {
	    shaid <- new ('shaid', type='collection', value=collection.uuid);
	    stopifnot (repo$exists (shaid));

	    bits <- c('labels','matrices','chms','collections');
	    val <- lapply (bits, function(x) repo$loadProperty(shaid, x));
	    names(val) <- bits;

	    val$repo <- repo;
	    val$basepath <- repo$blob.path (shaid);
	    val$shaid <- shaid;
	    val$uuid <- collection.uuid;
	    class(val) <- "ngchmCollection";
	    val
	}
    ));
    shaidyRegisterRepoAPI ("file", list (
	isLocal = function(repo) TRUE,
        "__super__" = "__generic__",
	addObjectToCollection = function (repo, collection, shaid) {
	    pl <- paste (shaid@type, "s", sep='');
	    if (!shaid@value %in% collection[[pl]]) {
		collection[[pl]] <- append (collection[[pl]], shaid@value);
		writeBinLines(jsonlite::toJSON(collection[[pl]],pretty=TRUE),
			   file.path (collection$basepath, paste (pl, ".json", sep='')));
	    }
	    collection
	},
	blobPath = function (repo, repoBase) {
	    stopifnot (file.exists (repoBase));
	    filename <- file.path (repoBase, "typeTab.json");
	    stopifnot (file.exists (filename));
	    typeTab <- jsonlite::fromJSON (readLines(filename, warn=FALSE));
	    stopifnot(!anyNA (typeTab$Type)); # NA is not allowed
	    stopifnot(all(nzchar(typeTab$Type))); # Empty string is not allowed
	    stopifnot(anyDuplicated(typeTab$Type) == 0); # Duplicates are not allowed
	    stopifnot(!anyNA (typeTab$Path)); # NA is not allowed
	    stopifnot(all(nzchar(typeTab$Path))); # Empty string is not allowed

	    # Create named vector of specified blob type paths.
	    paths <- typeTab$Path;
	    paths <- ifelse(substr(paths,1,1) == '/', paths, file.path(repoBase,paths));
	    names(paths) <- typeTab$Type;

	    # Return blob.path function for this repository
	    function (first, ...) {
		type <- if (is(first,"shaid")) first@type else first;
		d <- if (type %in% names(paths)) paths[type] else file.path(repoBase, type);
		if(is(first,"shaid")) d <- file.path (d, first@value);
		others <- c(lapply(list(...),function(item) {
		    if (is (item, "shaid")) {
			return (c(item@type, item@value));
		    } else {
			return (item);
		    }
		}), recursive=TRUE);
		do.call (file.path, as.list (c(d,others)))
	    }
	},
	copyBlobToLocalDir = function (repo, shaid, localDir) {
	    # Copy files in repo/shaid to existing local directory
	    srcblob <- repo$blob.path(shaid);
	    files <- dir (srcblob, recursive=TRUE, include.dirs=TRUE);
	    for (ff in files) {
		srcf <- file.path (srcblob, ff);
		dstf <- file.path (localDir, ff);
		if (file.info(srcf)$isdir) {
		    stopifnot (dir.create(dstf, recursive=FALSE));
		} else {
		    stopifnot(file.copy(srcf, dstf));
		}
	    }
	},
	exists = function (repo, shaid) {
	    dir.exists (repo$blob.path (shaid))
	},
	loadProperty = function (repo, shaid, propname) {
	    p <- repo$blob.path (shaid, sprintf ("%s.json", propname));
	    if (file.exists (p)) jsonlite::fromJSON(readLines(p, warn=FALSE), simplifyVector = FALSE) else c()
	},
	createCollection = function (repo, labels) {
	    collection.uuid <- getuuid (paste0(labels,rnorm(10),collapse=';'));
	    basepath <- repo$blob.path ('collection', collection.uuid);
	    stopifnot (!dir.exists (basepath));
	    stopifnot (dir.create (basepath));
	    if (nrow (labels) > 0) {
		writeBinLines(jsonlite::toJSON(labels,pretty=TRUE), file.path (basepath, "labels.json"));
	    }
            collection.uuid
	},
	# Add a collection reference to a collection
	#
	# The collection graph must be acyclic.
	#
	# @param collection A list containing details of a collection
	# @param uuid The uuid of the collection to add
	#
	# @return An updated list containing details of the collection
	#
	# @import jsonlite
	#
	# @export
	addCollectionToCollection = function (repo, collection, uuid) {
	    uuid.collection <- repo$loadCollection(uuid);
	    if (ngchmCollectionInCollection(uuid.collection, collection$uuid)) {
		stop (sprintf ("would form a cycle"));
	    }
	    collection$collections <- append (collection$collections, uuid);
	    writeBinLines(jsonlite::toJSON(collection$collections,pretty=TRUE),
		       file.path (collection$basepath, "collections.json"));
	    collection
	}

    ));
}

#' Create in memory shaid cache
#'
#' @param shaidyDir Basepath to a local shaidy repository.
#'
#' @return An in memory shaid cache
shaidyNewCache <- function (shaidyDir) {
    new.env()
};

#' Load the provid -> labels DB for a local shaidy repository.
#'
#' @param shaidyDir Basepath to a local shaidy repository.
#'
#' @return A shaidyProvidDB
shaidyLoadProvidDB <- function(shaidyDir) {
    db <- new.env();
    
    aaa <- function (provid, labels) {
        assign (provid, labels, envir=db)
    };

    ggg <- function (provid) {
        get (provid, envir=db, inherits=FALSE)
    };

    ggg0 <- function (provid) {
        get0 (provid, envir=db, inherits=FALSE)
    };

    pdb <- list (assign=aaa, get=ggg, get0=ggg0);
    class(pdb) <- "shaidyProvidDB";
    pdb
};

#' Load the provid -> shaid DB for a local shaidy repository
#'
#' @param shaidyDir Basepath to a local shaidy repository.
#'
#' @return A shaidyProvenanceDB
shaidyLoadProvenanceDB <- function(shaidyDir) {
    db <- new.env();
    
    aaa <- function (provid, shaid) {
	old <- get0 (provid, envir=db, inherits=FALSE);
	if (!shaid@value %in% old) {
	    assign (provid, append (old, shaid@value), envir=db, inherits=FALSE);
	}
    };

    ggg <- function (type, provids) {
	for (pp in provids) {
	    ids <- get0 (pp, envir=db, inherits=FALSE);
	    if (length(ids)>0) return (lapply (ids, function(x)new('shaid',type=type,value=x)));
	}
        NULL
    };

    pdb <- list (insert=aaa, get=ggg);
    class(pdb) <- "shaidyProvenanceDB";
    pdb
};

#' Load a shaidy repository
#'
#' @param accessMethod Method for accessing repository.
#' @param shaidyDir Basepath to shaidy repository.
#'
#' @return A shaidyRepo
#'
#' @export
shaidyLoadRepository <- function (accessMethod, shaidyDir) {
    accessMethod <- match.arg (accessMethod, names(shaidy.env$repoMethods));
    if ((accessMethod == 'file') && (Sys.info()[['sysname']] == "Windows"))  {
        shaidyDir <- gsub ("\\\\", "/", shaidyDir);
    }
    sr <- list (accessMethod = accessMethod,
                basepath = shaidyDir);
    class(sr) <- "shaidyRepo";
    sr$blob.path <- shaidy.env$repoMethods[[accessMethod]]$blobPath (sr, shaidyDir);
    sr$shaid.cache <- shaidyNewCache (shaidyDir);
    sr$providDB <- shaidyLoadProvidDB (shaidyDir);
    sr$provenanceDB <- shaidyLoadProvenanceDB (shaidyDir);
    sr
}

#' Print a shaidy repository
#'
#' @param x The shaidy repository to print
#' @param ... Unused extra parameters
#'
#' @return The shaidy repository
#'
#' @export
print.shaidyRepo <- function (x,...) {
    cat ("Shaidy repository at", x$basepath, "\n");
    invisible (x)
}

#' Create and initialize a local shaidy repository.
#'
#' @param shaidyDir Basepath of local shaidy repository to create
#' @param blob.types A string vector naming the blob types to include in the repository
#'
#' @import jsonlite
#'
#' @export
shaidyInitRepository <- function (shaidyDir, blob.types) {
    stopifnot (!dir.exists(shaidyDir));
    stopifnot (dir.create (shaidyDir, recursive=TRUE));
    typeTab <- data.frame (Type=blob.types, Path=blob.types);
    writeBinLines(jsonlite::toJSON(typeTab,pretty=TRUE), file.path (shaidyDir, "typeTab.json"));
    repo <- list (accessMethod='file', basepath=shaidyDir);
    class(repo) <- "shaidyRepo";
    repo$blob.path <- shaidy.env$repoMethods$file$blobPath (repo, shaidyDir)
    for (bt in blob.types) {
        stopifnot (dir.create (repo$blob.path (bt), recursive=FALSE));
    }
};

#' Find the first repository, if any, that contains the requested shaid
#'
#' @param repos The list of repositories to search
#' @param shaid The shaid to search for
#'
#' @return The first repository containing the shaid, otherwise NULL
#'
#' @export
shaidyFindRepo <- function (repos, shaid) {
    while (length (repos) > 0) {
        rr <- repos[[1]];
        if (rr$exists (shaid)) {
            return (rr);
        }
        repos <- repos[-1];
    }
    NULL
}

#' Create a provid from a list of label values
#'
#' @param ... shaidyRepo followed by a list of name=value labels to store in the provid
#'
#' @return A string containing the provid for the list of label values.
#'
#' @export
shaidyProvenance <- function (...) {
    shaidyRepo <- ..1;
    labels <- list (...)[-1];
    oo <- order (names (labels));
    provid <- gitSha (paste(sprintf ("%s=%s", names(labels)[oo], labels[oo]), collapse=','));
    shaidyRepo$providDB$assign (provid, labels);
    provid
}

#' Add data file(s) and properties to a local shaidy repository
#'
#' @param shaidyRepo The shaidy repository
#' @param blob.type The blob.type of the data file
#' @param blob.file Name of the file(s) within the blob
#' @param filename The filesystem path(s) to the file(s) to insert
#' @param properties A list of additional properties to save with the file(s)
#' @param shaid Shaid to store the blob as.
#'
#' @return The file's shaid
#'
#' @import jsonlite
#'
#' @export
shaidyAddFileBlob <- function (shaidyRepo, blob.type, blob.file, filename, properties=NULL, shaid=NULL) {
    stopifnot (length(blob.file)==length(filename),
               (!"properties.json" %in% blob.file) || (length(properties)==0),
               anyDuplicated(blob.file)==0);
    blobdir <- shaidyCreateProtoBlob(shaidyRepo,blob.type);
    if (length(properties) > 0) {
	props.json <- jsonlite::toJSON(properties);
	writeBinLines (props.json, file.path (blobdir, "properties.json"));
    }
    for (ii in 1:length(filename)) {
        stopifnot (file.copy (filename[[ii]], file.path (blobdir, blob.file[[ii]])));
    }
    if (length(shaid)==0) shaid <- shaidyHashProtoBlob(blob.type, blobdir);
    shaidyFinalizeProtoBlob (shaidyRepo,shaid,blobdir)
}

#' Create a prototype blob in a shaidy repository
#'
#' @param shaidyRepo The shaidy repository
#' @param blob.type The blob.type of the prototype blob
#'
#' @return The file path of the prototype blob
shaidyCreateProtoBlob <- function(shaidyRepo, blob.type) {
    protoblob <- utempfile("proto", tmpdir = shaidyRepo$blob.path(blob.type));
    dir.create (protoblob);
    protoblob
}

#' Finalize a prototype blob
#'
#' @param shaidyRepo The shaidy repository
#' @param shaid The shaid to assign the protoblob
#' @param protoblob The prototype blob to finalize
#'
#' @return The shaid (invisibly)
#'
#' The protoblob must have been created in the specified shaidy repository
#' and with the same blob type as the shaid.  When this function returns the
#' protoblob will no longer be accessible .  If a blob with the same shaid already
#' exists in this repository, the protoblob is quitely removed without affecting
#' the existing blob.
shaidyFinalizeProtoBlob <- function(shaidyRepo, shaid, protoblob) {
    typedir <- shaidyRepo$blob.path(shaid@type);
    stopifnot (substr(basename(protoblob),1,5)=="proto",
               protoblob==file.path(typedir,basename(protoblob)));
    blobpath <- file.path (typedir, shaid@value);
    if(file.exists(blobpath)) {
        unlink (protoblob, recursive=TRUE);
    } else {
        file.rename (protoblob, blobpath);
    }
    invisible(shaid)
}

#' Compute the shaid to assign a protoblob
#'
#' @param blob.type The blob.type of the prototype blob
#' @param protoblob The prototype blob
#'
#' @return The shaid to assign the protoblob
shaidyHashProtoBlob <- function(blob.type, protoblob) {
    stopifnot(file.exists(protoblob));
    if (file.info(protoblob)$isdir) {
        files <- sort (dir (protoblob, recursive=TRUE));
        hashes <- vapply (files, function(x)gitHashObject(file.path(protoblob,x)), "");
        value <- gitSha(paste(sprintf("%s=%s",files,hashes),collapse=','));
    } else {
        value <- gitHashObject (protoblob);
    }
    new ('shaid', type=blob.type, value=value)
}

#' Determine if one more blobs exist in a shaidy repository
#'
#' @param repo The shaidy repository
#' @param shaids A shaid or list of shaids
#'
#' @return a boolean vector
#'
#' @export
shaidyBlobExists <- function(repo, shaids) {
    if (is(shaids,"shaid")) {
        repo$exists (shaids)
    } else if (is(shaids,"list")) {
        vapply (shaids, function(sid)shaidyBlobExists(repo,sid), TRUE)
    } else {
        stop (sprintf("shaids has unknown class %s", class(shaids)));
    }
}

#' Copy a blob from one repository to another
#'
#' @param src The source repository
#' @param shaid The shaid of the blob to copy
#' @param dst The destination repository
#'
#' @return the shaid
#'
#' @export
shaidyCopyBlob <- function (src, shaid, dst) {
    stopifnot(shaidyBlobExists(src, shaid));
    if (shaidyBlobExists (dst, shaid)) return;

    if (dst$isLocal()) {
	dstblob <- shaidyCreateProtoBlob (dst, shaid@type);
	src$copyBlobToLocalDir (shaid, dstblob);
	shaidyFinalizeProtoBlob (dst, shaid, dstblob);
    } else if (src$isLocal()) {
	srcblob <- src$blob.path (shaid);
	dst$copyLocalDirToBlob (srcblob, shaid);
    } else {
	# Create a local copy of the blob.
        tmp <- ngchm.env$tmpShaidy;
	if (!shaidyBlobExists (tmp, shaid)) {
	    tmpblob <- shaidyCreateProtoBlob (tmp, shaid@type);
	    src$copyBlobToLocalDir (shaid, tmpblob);
	    shaidyFinalizeProtoBlob (tmp, shaid, tmpblob);
	}
	shaidyCopyBlob (tmp, shaid, dst);
    }
};
