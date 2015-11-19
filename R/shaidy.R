# Install a shaidy format NGCHM in a local shaidydir repository.
# This version assumes no one else is writing to the repository concurrently.
#

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

#' Return blob.path function for a local shaidy repository.
#'
#' @param shaidyDir Basepath to a local shaidy repository.
#'
#' @return a function (type, ...) that accepts a blob type and optionally
#'         additional file path components and returns a filepath
#'
#' @import jsonlite
#'
shaidyBlobPath <- function (shaidyDir) {
    # Load shaidyDir/typeTab.json and perform basic sanity checks.
    stopifnot (file.exists (shaidyDir));
    filename <- file.path (shaidyDir, "typeTab.json");
    stopifnot (file.exists (filename));
    typeTab <- jsonlite::fromJSON (readLines(filename, warn=FALSE));
    stopifnot(!anyNA (typeTab$Type)); # NA is not allowed
    stopifnot(all(nzchar(typeTab$Type))); # Empty string is not allowed
    stopifnot(anyDuplicated(typeTab$Type) == 0); # Duplicates are not allowed
    stopifnot(!anyNA (typeTab$Path)); # NA is not allowed
    stopifnot(all(nzchar(typeTab$Path))); # Empty string is not allowed

    # Create named vector of specified blob type paths.
    paths <- typeTab$Path;
    paths <- ifelse(substr(paths,1,1) == '/', paths, file.path(shaidyDir,paths));
    names(paths) <- typeTab$Type;

    # Return blob.path function for this repository
    function (type, ...) {
	d <- if (type %in% names(paths)) paths[type] else file.path(shaidyDir, type);
	file.path (d, ...)
    }
};

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

#' Load a local shaidy repository
#'
#' @param shaidyDir Basepath to a local shaidy repository.
#'
#' @return A shaidyRepo
#'
#' @export
shaidyLoadRepository <- function (shaidyDir) {
    sr <- list (basepath = shaidyDir,
                blob.path = shaidyBlobPath (shaidyDir),
                shaid.cache = shaidyNewCache (shaidyDir),
                providDB = shaidyLoadProvidDB (shaidyDir),
                provenanceDB = shaidyLoadProvenanceDB (shaidyDir));
    class(sr) <- "shaidyRepo";
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
    writeLines(jsonlite::toJSON(typeTab,pretty=TRUE), file.path (shaidyDir, "typeTab.json"));
    blob.path <- shaidyBlobPath (shaidyDir);
    for (bt in blob.types) {
        stopifnot (dir.create (blob.path (bt), recursive=FALSE));
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
        if (file.exists (repos[[1]]$blob.path (shaid@type, shaid@value))) {
            return (repos[[1]]);
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
	writeLines (props.json, file.path (blobdir, "properties.json"));
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
    protoblob <- tempfile("proto", tmpdir = shaidyRepo$blob.path(blob.type));
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
