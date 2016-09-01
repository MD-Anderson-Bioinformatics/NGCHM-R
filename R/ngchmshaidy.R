
#' Initialize shaidy subsystem for NGCHMs
#'
ngchmShaidyInit <- function() {
    shaidyRegisterRepoAPI ("http", (function(fileMethods) list(
        "__super__" = fileMethods,
	blobPath = function (repo, repoBase) {
	    resp <- GET (repoBase);
	    stopifnot (resp$status_code == 200);
	    tarfile <- utempfile ("shaidcache", fileext='.tar');
	    local <- utempfile ("shaidcache");
	    stopifnot (dir.create (local, recursive=TRUE));
	    writeBin (resp$content, tarfile);
	    systemCheck (sprintf ("tar xf %s -C %s", tarfile, local));
	    unlink (tarfile);
	    fileMethods$blobPath (repo, local)
	}
    )) (shaidyRepoAPI('file')));

    tokenStash <- new.env (parent=emptyenv());

    shaidyRegisterRepoAPI ("api", list (
        "__super__" = "__generic__",
	isLocal = function(repo) FALSE,
        getToken = function (repo) {
            bp <- repo$blob.path("");
            if (exists (bp, envir=tokenStash)) {
                tokenStash[[bp]]
            } else {
                repo$getNewToken ();
            }
        },

        setToken = function (repo, token) {
            tokenStash[[repo$blob.path("")]] <- token;
            tokenStash[[repo$blob.path("")]]
        },

        getNewToken = function (repo) {
            cat ("Enter access token: ", file=stderr());
            tokenStash[[repo$blob.path("")]] <- readLines (n=1);
            tokenStash[[repo$blob.path("")]]
        },

	addObjectToCollection = function (repo, collection, shaid) {
	    uri <- collection$repo$blob.path('insert', shaid, collection$shaid);
	    resp <- POST (uri, add_headers(Authorization=repo$getToken()));
	    while (resp$status_code == 401) {
	        resp <- POST (uri, add_headers(Authorization=repo$getNewToken()));
            }
	    stopifnot (resp$status_code == 200);
	    collection$repo$loadCollection(collection$uuid)
	},
        renderChm = function (repo, shaid) {
	    uri <- repo$blob.path('render', shaid);
	    resp <- POST (uri, add_headers(Authorization=repo$getToken()));
	    while (resp$status_code == 401 || resp$status_code == 504) {
                if (resp$status_code == 504) {
                    cat ("Waiting for render to complete.\n", file=stderr());
	            resp <- POST (uri, add_headers(Authorization=repo$getToken()));
                } else {
	            resp <- POST (uri, add_headers(Authorization=repo$getNewToken()));
                }
            }
            cat ('Render', shaid@type, shaid@value, 'status:', resp$status_code, '\n', file=stderr());
        },
	blobPath = function (repo, repoBase) {
	    return (function (first, ...) {
		type <- if (is(first,"shaid")) first@type else first;
		uri <- paste (repoBase, type, sep='/');
		if(is(first,"shaid")) uri <- paste (uri, first@value, sep='/');
		others <- c(lapply(list(...),function(item) {
		    if (is (item, "shaid")) {
			return (c(item@type, item@value));
		    } else {
			return (item);
		    }
		}), recursive=TRUE);
		paste (c(uri, others), sep='/', collapse='/')
	    });
	},
	copyLocalDirToBlob = function (repo, localDir, shaid) {
	    dstblob <- repo$blob.path('tar', shaid);
	    tarfile <- utempfile ("shaidcache", fileext='.tar');
	    systemCheck (sprintf ("tar cf %s -C %s .", tarfile, localDir));
	    resp <- PUT (dstblob, add_headers(Authorization=repo$getToken()), body=upload_file(tarfile));
	    while (resp$status_code == 401) {
	        resp <- PUT (dstblob, add_headers(Authorization=repo$getNewToken()), body=upload_file(tarfile));
            }
	    stopifnot (resp$status_code == 200);
	    unlink (tarfile);
	},
	copyBlobToLocalDir = function (repo, shaid, localDir) {
	    srcblob <- repo$blob.path('tar', shaid);
	    resp <- GET (srcblob);
	    if (resp$status_code == 401) {
	        resp <- GET (srcblob, add_headers(Authorization=repo$getToken()));
            }
	    while (resp$status_code == 401) {
	        resp <- GET (srcblob, add_headers(Authorization=repo$getNewToken()));
            }
	    stopifnot (resp$status_code == 200);
	    tarfile <- utempfile ("shaidcache", fileext='.tar');
	    writeBin (resp$content, tarfile);
	    systemCheck (sprintf ("tar xf %s -C %s", tarfile, localDir));
	    unlink (tarfile);
	},
	exists = function (repo, shaid) {
	    uri <- repo$blob.path (shaid);
	    uri <- sub ('api/', 'api/exists?ids=', uri);
	    resp <- GET (uri);
	    if (resp$status_code == 401) {
	        resp <- GET (uri, add_headers(Authorization=repo$getToken()));
            }
	    while (resp$status_code == 401) {
	        resp <- GET (uri, add_headers(Authorization=repo$getNewToken()));
            }
	    return (length (ngchmResponseJSON(resp)$data) > 0);
	},
	loadProperty = function (repo, shaid, propname) {
	    uri <- repo$blob.path ('prop', propname, shaid);
	    resp <- GET (uri);
	    if (resp$status_code == 401) {
	        resp <- GET (uri, add_headers(Authorization=repo$getToken()));
            }
	    while (resp$status_code == 401) {
	        resp <- GET (uri, add_headers(Authorization=repo$getNewToken()));
            }
	    if (status_code(resp) == 200) ngchmResponseJSON(resp)$data else c()
	},
	createCollection = function (repo, labels) {
	    uri <- repo$blob.path ('create', 'collection');
	    resp <- POST (uri, add_headers(Authorization=repo$getToken()), body=list(labels=labels), encode="json");
	    while (resp$status_code == 401) {
	        resp <- POST (uri, add_headers(Authorization=repo$getNewToken()), body=list(labels=labels), encode="json");
            }
	    if (status_code(resp) == 200) ngchmResponseJSON(resp)$data else c()
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
            repo$addObjectToCollection (collection, new('shaid',type="collection",value=uuid))
	}

    ));

    shaidyDir <- utempfile ("shaidy");
    ngchmInitShaidyRepository (shaidyDir);
    ngchm.env$tmpShaidy <- shaidyLoadRepository ('file', shaidyDir);
    ngchm.env$tmpShaidyStack <- c();
    ngchm.env$shaidyStack <- c();
}

#' Create a shaidy repository for NG-CHMS
#'
#' @param shaidyDir Basepath of local shaidy repository to create
#'
#' @export
ngchmInitShaidyRepository <- function (shaidyDir) {
    shaidyInitRepository (shaidyDir, c("collection", "chm", "dataset", "dendrogram", "label", "tile", "viewer", "file"))
}

#' Push a shaidy repository onto the stack of temporary repositories
#'
#' @param shaidyDir Basepath of local shaidy repository to use as a temporary repository
#'
#' @export
ngchmPushTempRepository <- function (shaidyDir) {
    newrepo <- shaidyLoadRepository ('file', shaidyDir);
    ngchm.env$tmpShaidyStack <- c(list(ngchm.env$tmpShaidy), ngchm.env$tmpShaidyStack);
    ngchm.env$tmpShaidy <- newrepo
}

#' Push a shaidy repository onto the stack of source repositories
#'
#' @param shaidyDir Basepath of local shaidy repository to use as a source repository
#' @param accessMethod Method for accessing repository
#'
#' @export
ngchmPushSourceRepository <- function (shaidyDir, accessMethod='file') {
    newrepo <- shaidyLoadRepository (accessMethod, shaidyDir);
    ngchm.env$shaidyStack <- c(list(newrepo), ngchm.env$shaidyStack);
}

#' Find a repository, if any, that contains the requested shaid
#'
#' @param shaid The shaid to search for
#' @param required Abort if requireed and shaid not found in a known repo
#'
#' @return The first repository containing the shaid, otherwise NULL.  The
#'         temporary repositories are searched before source repositories.
#'
#' @export
ngchmFindRepo <- function (shaid, required=TRUE) {
    repo <- shaidyFindRepo (c(list(ngchm.env$tmpShaidy),ngchm.env$tmpShaidyStack,ngchm.env$shaidyStack), shaid);
    if (required && length(repo)==0) {
        stop (sprintf ("Shaid %s %s not found in any known repository", shaid@type, shaid@value));
    }
    repo
}

#' Create a new collection in a local shaidy repository
#'
#' @param shaidyRepo The shaidy repository
#' @param labels Initial labels for collection (a data.frame of (Name,Value) tuples)
#'
#' @return a string containing the UUID of the newly created repository
#'
#' @import jsonlite
#'
#' @export
ngchmNewCollection <- function (shaidyRepo, labels=data.frame()) {
    labels <- as.data.frame (labels);
    if (nrow (labels) > 0) {
        stopifnot ("Name" %in% colnames(labels));
        stopifnot ("Value" %in% colnames(labels));
    }
    shaidyRepo$createCollection (labels)
};

#' Recursively determine if collection uuid is contained in collection
#' A collecton always contains itself.
#'
#' @param collection A list containing details of a collection
#' @param uuid A string containing the UUID to check
#'
#' @return TRUE iff collection contains uuid, otherwise FALSE
#'
#' @export
ngchmCollectionInCollection <- function (collection, uuid) {
    if (collection$uuid == uuid) return (TRUE);
    if (uuid %in% collection$collections) return (TRUE);
    shaidyRepo <- collection$repo;
    todo <- collection$collections;
    done <- collection$uuid;
    while (length (todo) > 0) {
	collect.uuid <- todo[[1]];
	todo <- todo[-1];
	if (!collect.uuid %in% done) {
            collection <- shaidyRepo$loadCollection(collect.uuid);
            if (uuid %in% collection$collections) return (TRUE);
	    todo <- append (collection$collections, todo);
	    done <- append (done, collection$uuid);
	}
    }
    return (FALSE);
}

#' Add a matrix reference to a collection
#'
#' @param collection A list containing details of a collection
#' @param name The name to associate with the matrix reference
#' @param shaid The shaid of the matrix to add to the collection
#'
#' @return An updated list contaiing details of the collection
#'
#' @import jsonlite
#'
#' @export
ngchmAddMatrixToCollection <- function (collection, name, shaid) {
    mats <- collection$matrices;
    if (!any(mats$Name==name)) {
        mats <- rbind (mats, data.frame(Name=name, Shaid=shaid@value));
        writeLines(jsonlite::toJSON(mats,pretty=TRUE),
	           file.path (collection$basepath, "matrices.json"));
        collection$matrices <- mats;
    }
    collection
};

#' Add an object reference to a collection
#'
#' @param repo The repository containing the collection
#' @param uuid A collection uuid
#' @param shaid The shaid of the object to add to the collection
#'
#' @return An updated list containing details of the collection
#'
#' @import jsonlite
#'
#' @export
ngchmAddObjectToCollection <- function (repo, uuid, shaid) {
    stopifnot (is(shaid,"shaid"),
               shaid@type %in% c('chm','dataset','label','collection'));
    collection <- repo$loadCollection(uuid);
    repo$addObjectToCollection (collection, shaid)
};

#' Render a shaidy NGCHM
#'
#' @param repo The repository containing the chm
#' @param shaid The shaid of the chm to render
#'
#' @return Nothing
#'
#' @export
ngchmRenderChm <- function (repo, shaid) {
    stopifnot (is(shaid,"shaid"),
               shaid@type %in% c('chm'));
    repo$renderChm (shaid)
};

#' Create a recursive description of a collection
#'
#' @param collection A list containing details of a collection
#' @param depth The indentation depth to use
#'
#' @return a string vector describing the contents of the collection
#'
#' @export
ngchmCollectionTree <- function (collection, depth=0) {
    indent <- paste0 (rep ("  ", depth), collapse='');
    c(sprintf ("%scollection %s", indent, collection$uuid),
      sprintf ("%s  label %s=%s", indent, collection$labels$Name, collection$labels$Value),
      sprintf ("%s  matrix %s=%s", indent, collection$matrices$Name, collection$matrices$Shaid),
      sprintf ("%s  ngchm %s", indent, collection$chms),
      lapply (collection$collections, function(uuid) {
          ngchmCollectionTree (collection$repo$loadCollection(uuid), depth+1)
      }),
      recursive=TRUE)
}

#' Compute shaid for a data file
#'
#' @param format The format of the data file
#' @param filename The filesystem path to the data file
#'
#' @return The shaid of the data file
#'
#' @import jsonlite
#'
#' @export
ngchmGetDataFileShaid <- function (format, filename) {
    stopifnot (format == 'tsv');
    gid <- gitHashObject (filename);
    coreproperties <- list (format=format);
    props.json <- jsonlite::toJSON(coreproperties);
    new ('shaid', type='dataset', value=gitSha (paste('dataset',props.json,gid,sep='',collapse='')))
}

#' Add a data file to a local shaidy repository
#'
#' @param shaidyRepo The shaidy repository
#' @param format The format of the data file
#' @param filename The filesystem path to the data file
#' @param properties A list of additional properties to save with file
#'
#' @return The file's shaid
#'
#' @import jsonlite
#'
#' @export
ngchmAddDatasetBlob <- function (shaidyRepo, format, filename, properties=NULL) {
    stopifnot (format == 'tsv');
    if (format == 'tsv') {
       index.filename <- utempfile ("index", fileext='.tsv');
       tsvio::tsvGenIndex (filename, index.filename);
       blobfiles = c('matrix.tsv', 'index.tsv');
       filenames = c(filename, index.filename);
    }
    shaidyAddFileBlob (shaidyRepo, 'dataset', blobfiles, filenames,
                       properties=c(list(format=format),properties))
}

#' Save a numeric matrix as a blob in a shaidy repository
#'
#' @param shaidyRepo The shaidy repository
#' @param format The format in which to save the matrix
#' @param mat The data matrix
#'
#' @return The shaid of the saved blob
#'
#' @export
ngchmSaveAsDatasetBlob <- function (shaidyRepo, format, mat) {
    if (is (mat, 'shaid')) return (mat);
    stopifnot (format == 'tsv',
               is (mat, 'matrix'),
	       length (dim(mat)) == 2,
	       length (rownames(mat)) > 0,
	       length (colnames(mat)) > 0);
    filename <- utempfile ("matrix", fileext='.tsv');
    write.table (mat, filename, quote=FALSE, sep='\t');
    class(format) <- 'singleElement';
    props <- list(nrow=nrow(mat),ncol=ncol(mat));
    class(props[[1]]) <- 'singleElement';
    class(props[[2]]) <- 'singleElement';
    shaid <- ngchmAddDatasetBlob (shaidyRepo, format, filename, props);
    unlink (filename);
    shaid
}

#' Load a data matrix from a local shaidy repository
#'
#' @param shaidyRepo The shaidy repository
#' @param shaid The shaid of the dataset blob to load
#' @param datatype Prototype of matrix data elements (defaults to 0.0)
#'
#' @return a list containing details of the loaded dataset
#'
#' @import jsonlite
#'
#' @export
ngchmLoadDatasetBlob <- function (shaidyRepo, shaid, datatype) {
    blobdir <- shaidyRepo$blob.path ('dataset', shaid@value);
    props <- jsonlite::fromJSON (readLines (file.path (blobdir, "properties.json")));
    mat <- suppressWarnings (tsvio::tsvGetData (file.path (blobdir, "matrix.tsv"), file.path (blobdir, "index.tsv"),
                              NULL, NULL, if (missing(datatype)) 0.0 else datatype));
    list (shaid=shaid, properties=props, mat=mat)
}

writeHCDataTSVs <- function(uDend, theOutputHCDataFileName, theOutputHCOrderFileName)
{
    if (is(uDend,'dendrogram')) uDend <- as.hclust(uDend);
    stopifnot (is (uDend, 'hclust'));
    data <- cbind(uDend$merge, uDend$height, deparse.level=0);
    colnames(data)<-c("A", "B", "Height")
    ###Write out the data as a Tab separated file to the specified location
    write.table(data, file = theOutputHCDataFileName, append = FALSE, quote = FALSE, sep = "\t", row.names=FALSE)

    data <- t(vapply(1:length(uDend$labels),function(i)c(uDend$labels[i],which(uDend$order==i)),c("a",1)));
    colnames(data) <- c("Id", "Order")
    ###Write out the order data as a Tab separated file to the specified location (1 more row than data file)
    write.table(data, file = theOutputHCOrderFileName, append = FALSE, quote = FALSE, sep = "\t", row.names=FALSE)
}

ngchmSaveTemplateAsBlob <- function (shaidyRepo, source.path, dest.path, substitutions) {
    blobdir <- shaidyCreateProtoBlob(shaidyRepo,'file');
    writeTemplate(source.path, dest.path, substitutions, blobdir);
    shaid <- shaidyHashProtoBlob('file', blobdir);
    shaidyFinalizeProtoBlob(shaidyRepo, shaid, blobdir)
}

#' Save a dendrogram as a blob in a shaidy repository
#'
#' @param shaidyRepo The shaidy repository
#' @param ddg The dendrogram
#'
#' @return The shaid of the saved blob
#'
#' @export
ngchmSaveAsDendrogramBlob <- function (shaidyRepo, ddg) {
    if (is (ddg, 'shaid')) return (ddg);
    stopifnot (is (ddg, 'dendrogram'));
    datafilename <- utempfile ("ddg", fileext='.txt');
    orderfilename <- utempfile ("ddg", fileext='.txt');
    writeHCDataTSVs (ddg, datafilename, orderfilename);
    ddgfilename <- utempfile ("ddg", fileext='.str');
    sink(ddgfilename);
    str(ddg);
    sink(NULL);
    rdafilename <- utempfile ("ddg", fileext='.rda');
    save (ddg, file=rdafilename);
    shaid <- shaidyAddFileBlob (shaidyRepo, 'dendrogram',
                                c('dendrogram-data.tsv', 'dendrogram-order.tsv', 'dendrogram.str', 'dendrogram.rda'),
                                c(datafilename, orderfilename, ddgfilename, rdafilename));
    unlink (datafilename);
    unlink (orderfilename);
    unlink (ddgfilename);
    unlink (rdafilename);
    shaid
}

#' @export
as.dendrogram.shaid <- function (shaid) {
    stopifnot (is(shaid,"shaid"), shaid@type=='dendrogram');
    repo <- ngchmFindRepo (shaid);
    ee <- new.env();
    load (repo$blob.path (shaid, 'dendrogram.rda'), ee);
    stopifnot (exists ('ddg', ee));
    return (get ('ddg', ee));
}

#' @export
as.hclust.shaid <- function (shaid) {
   as.hclust (as.dendrogram (shaid));
}

#' Row center a shaidy dataset
#'
#' @param shaidyRepo The shaidy repository
#' @param shaid The shaid of the dataset to row center
#'
#' @return A list of shaids for the row centered dataset
ngchmRowCenter <- function (shaidyRepo, shaid) {
    provid <- shaidyProvenance (shaidyRepo, name="ngchmRowCenter", shaid=shaid@value);
    res <- shaidyRepo$provenanceDB$get ('dataset',provid);
    if (length(res) == 0) {
        ds <- ngchmLoadDatasetBlob (shaidyRepo, shaid);
	mm <- rowMeans (ds$mat, na.rm=TRUE);
	res <- list(ngchmSaveAsDatasetBlob (shaidyRepo, 'tsv', ds$mat-mm));
	shaidyRepo$provenanceDB$insert (provid, res[[1]]);
    }
    res
}

#' Make a shaidy format NGCHM.
#'
#' @param chm The shaidy format CHM to compile.
#' @return The CHM
ngchmMakeFormat.shaidy <- function (chm) {
    chm
}

#' Get the axis labels of a shaidy dataset or dendrogram
#'
#' @param shaid The shaid of the dataset or dendrogram to get the labels of
#' @param axis For datasets, the axis of the labels to get
#'
#' @return a list of shaids containing the labels
#'
#' @export
ngchmGetLabels <- function (shaid, axis=NULL) {
    stopifnot (is(shaid,"shaid"),
               (shaid@type=='dendrogram') || (axis %in% c("row","column")));
    shaidyRepo <- ngchm.env$tmpShaidy;
    provid <- shaidyProvenance (shaidyRepo, name="ngchmGetLabels", type=shaid@type, shaid=shaid@value, axis=axis);
    res <- shaidyRepo$provenanceDB$get ('label', provid);
    if (length(res) == 0) {
        srcRepo <- ngchmFindRepo (shaid);
        if (shaid@type == 'dataset') {
            ds <- ngchmLoadDatasetBlob (srcRepo, shaid);
            labels <- (if (axis=="row") rownames else colnames)(ds$mat);
        } else if (shaid@type == 'dendrogram') {
	    oo <- read.delim (srcRepo$blob.path(shaid@type,shaid@value,"dendrogram-order.tsv"), header=TRUE, colClasses=c('character','numeric'));
	    labels <- oo$Id[order(oo$Order)]
        } else {
	    stop (sprintf ("Unknown shaid type %s", shaid@type));
        }
	res <- list(ngchmSaveLabelsAsBlob (shaidyRepo, labels));
	shaidyRepo$provenanceDB$insert (provid, res[[1]]);
    }
    res
}

ngchmSaveLabelsAsBlob <- function (shaidyRepo, labels) {
    filename <- utempfile ("label", fileext='.txt');
    writeLines (labels, filename);
    res <- shaidyAddFileBlob (shaidyRepo, 'label', 'labels.txt', filename);
    unlink (filename);
    res
}

#' Get the axis labels of a shaidy dataset or dendrogram
#'
#' @param shaid The shaid of the dataset or dendrogram to get the labels of
#' @param axis For datasets, the axis of the labels to get
#'
#' @return A string vector containing the axis labels of the dataset or dendrogram
#'
#' @export
ngchmGetLabelsStr <- function (shaid, axis=NULL) {
    res <- ngchmGetLabels (shaid, axis);
    blobfile <- ngchm.env$tmpShaidy$blob.path (res[[1]]@type, res[[1]]@value, 'labels.txt');
    readLines (blobfile)
}

#' Save an NGCHM as a shaidy blob
#'
#' @param shaidyRepo The shaidy repository to write to
#' @param chm The NGCHM to write
#'
#' @return The shaid of the saved NGCHM
ngchmSaveChmAsBlob <- function (shaidyRepo, chm) {
    stopifnot (is(shaidyRepo,"shaidyRepo"),
               is(chm,"ngchm"));
    blob <- shaidyCreateProtoBlob (shaidyRepo, 'chm');
    writeChm (chm, blob);
    writeChmPost (chm, blob);
    shaid <- shaidyHashProtoBlob ('chm', blob);
    shaidyFinalizeProtoBlob (shaidyRepo, shaid, blob)
}

#' Get the tiles for a shaidy dataset
#'
#' @param repo The shaidy repository in which to create the tile
#' @param dataset The shaid of the dataset to tile
#' @param rowOrder The row order of the tiles
#' @param colOrder The column order of the tiles
#'
#' @return a list of shaids containing the tiles
#'
#' @export
ngchmTileDataset <- function (repo, dataset, rowOrder, colOrder) {
    stopifnot (is(repo,"shaidyRepo"),
               is(dataset,"shaid"), is(rowOrder,"shaid"), is(colOrder,"shaid"),
               rowOrder@type %in% c("label","dendrogram"),
               colOrder@type %in% c("label","dendrogram"));

    if (rowOrder@type=="dendrogram") rowOrder <- ngchmGetLabels(rowOrder,"row")[[1]];
    if (colOrder@type=="dendrogram") colOrder <- ngchmGetLabels(colOrder,"column")[[1]];
    provid <- shaidyProvenance (repo, name="tileDataset", dataset=dataset@value, rowOrder=rowOrder@value, colOrder=colOrder@value);
    res <- repo$provenanceDB$get ('tile', provid);
    if (length(res) == 0) {
        blob <- shaidyCreateProtoBlob (repo, 'tile');
	stopifnot (system2 ("tiledata", args=c(vapply(c(dataset,rowOrder,colOrder),repo$blob.path,""),blob)) == 0);
        shaid <- shaidyHashProtoBlob ('tile', blob);
        shaidyFinalizeProtoBlob (repo, shaid, blob)
	repo$provenanceDB$insert (provid, shaid);
        res <- list(shaid)
    }
    res
}

#' Get the user's current collection
#'
#' @return the identity of the current collection
#'
#' @export
chmCurrentCollection <- function () {
    if (length(ngchm.env$currentServer) == 0 && length(ngchm.env$servers) > 0) {
	chmSetCollection (getOption("NGCHM.Collection", "//"));
    }
    return (ngchm.env$currentCollection);
}

#' Get the user's current server
#'
#' @return the identity of the current server
#'
#' @export
chmCurrentServer <- function () {
    if (length(ngchm.env$currentServer) == 0 && length(ngchm.env$servers) > 0) {
	chmSetCollection (getOption("NGCHM.Collection", "//"));
    }
    return (ngchm.env$currentServer);
}

#' Set the user's current server and/or collection
#'
#' The path is a sequence of components separated by slashes (/).
#' If the path begins with a double slash (//) the following
#' component is interpreted as a server name. If the server name is
#' omitted (i.e. empty) the default server will be used.  If the path
#' does not begin with a double slash, the current server will be used.
#'
#' If the path begins with a slash, the components (following the
#' server, if specified) are interpreted relative to
#' the root collection of the server concerned.  Otherwise, they
#' are interpreted relative to the current collection.
#'
#' The interpretation of each path component is server specific.
#'
#' @param path A path specifying a server and/or collection
#'
#' @export
chmSetCollection <- function (path) {
    stopifnot (!missing(path) && typeof(path)=="character" && length(path)==1);
    res <- parsePathSpec (path);
    ngchm.env$currentServer <- res$server;
    ngchm.env$currentCollection <- res$collection;
}

parsePathSpec <- function (path) {
    newServer <- ngchm.env$currentServer;
    if (length(newServer) == 0 || newServer=="") {
        newServer <- chmListServers()[1];
    }
    newCollection <- ngchm.env$currentCollection;
    if (length(newCollection) == 0) {
        newCollection <- "";
    }
    parts <- strsplit (path, "/")[[1]];
    if (length(parts) > 1 && parts[1]=="" && parts[2]=="") {
        parts <- parts[c(-1,-2)];
        if (length(parts) > 0 && parts[1]!="") {
            newServer <- parts[1];
        }
        if (is.na(newServer) || !(newServer %in% chmListServers())) {
            stop ("cannot set server: ", newServer);
        }
        if (length(parts) > 0) parts <- parts[-1];
        newCollection <- "";
    } else if (length(parts) > 0 && parts[1] == "") {
        parts <- parts[-1];
        newCollection <- "";
    }
    if (length(parts) > 0) {
        server <- chmServer (newServer);
        newCollection <- server@serverProtocol@findCollection (server, newCollection, parts);
        if (length(newCollection)==0) {
            stop ("cannot find collection: ", path);
        }
    }
    list (server=newServer, collection=newCollection)
}

#' Create a new collection
#'
#' The path is a sequence of components separated by slashes (/).
#' If the path begins with a double slash (//) the following
#' component is interpreted as a server name. If the server name is
#' omitted (i.e. empty) the default server will be used.  If the path
#' does not begin with a double slash, the current server will be used.
#'
#' If the path begins with a slash, the components (following the
#' server, if specified) are interpreted relative to
#' the root collection of the server concerned.  Otherwise, they
#' are interpreted relative to the current collection.
#'
#' The interpretation of each path component is server specific.
#'
#' @param path A path specifying a collection to be created
#' @param recursive If TRUE, create intermediate collections as required
#'
#' @export
chmCreateCollection <- function (path, recursive=FALSE) {
    stopifnot (!missing(path) && typeof(path)=="character" && length(path)==1);
    server <- ngchm.env$currentServer;
    collection <- ngchm.env$currentCollection;
    parts <- strsplit (path, "/")[[1]];
    if (length(parts) > 1 && parts[1]=="" && parts[2]=="") {
        parts <- parts[c(-1,-2)];
        if (length(parts) == 0 || parts[1]=="") {
	    server <- chmListServers()[1];
        } else {
            server <- parts[1];
        }
	if (is.na(server) || !(server %in% chmListServers())) {
	    stop ("cannot find server: ", server);
	}
	if (length(parts) > 0) parts <- parts[-1];
	collection <- "";
    } else if (length(parts) > 0 && parts[1] == "") {
        parts <- parts[-1];
	collection <- "";
    }
    if (length(parts) > 0) {
        server <- chmServer (server);
	subCollection <- server@serverProtocol@findCollection (server, collection, parts[1]);
        while (length(subCollection) != 0) {
            parts <- parts[-1];
            collection <- subCollection;
            if (length(parts) == 0) {
                subCollection <- NULL;
            } else {
	        subCollection <- server@serverProtocol@findCollection (server, collection, parts[1]);
            }
        }
    }
    if (length(parts) == 0) {
        stop ("collection ", path, " exists");
    } else if (!recursive && (length(parts) > 1)) {
        stop ("collection ", path, " does not exist");
    } else {
        while (length (parts) > 0) {
	    collection <- server@serverProtocol@createCollection (server, collection, parts[1]);
            if (length(collection) == 0) {
                stop ("cannot create collection ", path);
            }
            parts <- parts[-1];
        }
    }
}
