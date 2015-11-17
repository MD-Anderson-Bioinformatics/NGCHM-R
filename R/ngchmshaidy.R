#' Create a shaidy repository for NG-CHMS
#'
#' @param shaidyDir Basepath of local shaidy repository to create
#'
#' @export
ngchmInitShaidyRepository <- function (shaidyDir) {
    shaidyInitRepository (shaidyDir, c("collection", "chm", "dataset", "tile"))
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
    collection.uuid <- getuuid (paste0(labels,collapse=';'));
    basepath <- shaidyRepo$blob.path ('collection', collection.uuid);
    stopifnot (!dir.exists (basepath));
    stopifnot (dir.create (basepath));
    if (nrow (labels) > 0) {
        writeLines(jsonlite::toJSON(labels,pretty=TRUE), file.path (basepath, "labels.json"));
    }
    collection.uuid
};

#' Load a collection from a local shaidy repository
#'
#' @param shaidyRepo The shaidy repository
#' @param collection.uuid A string containing the UUID of the collection to load.
#'
#' @return a list containing the collection data
#'
#' @import jsonlite
#'
#' @export
ngchmLoadCollection <- function (shaidyRepo, collection.uuid="") {
    basepath <- shaidyRepo$blob.path ('collection', collection.uuid);
    stopifnot (file.exists (basepath));
    ld <- function (f) {
        p <- file.path (basepath, f);
        if (file.exists (p)) jsonlite::fromJSON(readLines(p, warn=FALSE)) else c()
    };
    bits <- c('labels','matrices','chms','collections');
    val <- lapply (bits, function(x) ld(sprintf("%s.json",x)));
    names(val) <- bits;
    val$shaidyRepo <- shaidyRepo;
    val$basepath <- basepath;
    val$uuid <- collection.uuid;
    val
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
    shaidyRepo <- collection$shaidyRepo;
    todo <- collection$collections;
    done <- collection$uuid;
    while (length (todo) > 0) {
	collect.uuid <- todo[[1]];
	todo <- todo[-1];
	if (!collect.uuid %in% done) {
            collection <- ngchmLoadCollection (shaidyRepo, collect.uuid);
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

#' Add a CHM reference to a collection
#'
#' @param collection A list containing details of a collection
#' @param shaid The shaid of the CHM to add to the collection
#'
#' @return An updated list containing details of the collection
#'
#' @import jsonlite
#'
#' @export
ngchmAddChmToCollection <- function (collection, shaid) {
    if (!shaid %in% collection$chms) {
        collection$chms <- append (collection$chms, shaid@value);
        writeLines(jsonlite::toJSON(collection$chms,pretty=TRUE),
	           file.path (collection$basepath, "chms.json"));
    }
    collection
};

#' Add a collection reference to a collection
#'
#' The collection graph must be acyclic.
#'
#' @param collection A list containing details of a collection
#' @param uuid The uuid of the collection to add
#'
#' @return An updated list containing details of the collection
#'
#' @import jsonlite
#'
#' @export
ngchmAddCollectionToCollection <- function (collection, uuid) {
    uuid.collection <- ngchmLoadCollection (collection$shaidyRepo, uuid);
    if (ngchmCollectionInCollection(uuid.collection, collection$uuid)) {
        stop (sprintf ("would form a cycle"));
    }
    collection$collections <- append (collection$collections, uuid);
    writeLines(jsonlite::toJSON(collection$collections,pretty=TRUE),
	       file.path (collection$basepath, "collections.json"));
    collection
}

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
          ngchmCollectionTree (ngchmLoadCollection (collection$shaidyRepo, uuid), depth+1)
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
    new ('shaid', value=gitSha (paste('dataset',props.json,gid,sep='',collapse='')))
}

#' Add a data file to a local shaidy repository
#'
#' @param shaidyRepo The shaidy repository
#' @param format The format of the data file
#' @param filename The filesystem path to the data file
#'
#' @return The file's shaid
#'
#' @import jsonlite
#'
#' @export
ngchmAddDatasetBlob <- function (shaidyRepo, format, filename) {
    stopifnot (format == 'tsv');
    shaid <- ngchmGetDataFileShaid (format, filename);
    blobdir <- shaidyRepo$blob.path ('dataset', shaid@value);
    if (!dir.exists (blobdir)) {
        dir.create (blobdir);
        properties <- list (format=format);
        props.json <- jsonlite::toJSON(properties);
	writeLines (props.json, file.path (blobdir, "properties.json"));
	stopifnot (file.copy (filename, file.path (blobdir, "matrix.tsv")));
    }
    shaid
}

#' Save a matrix as a blob in a shaidy repository
#'
#' @param shaidyRepo The shaidy repository
#' @param format The format in which to save the matrix
#' @param mat The data matrix
#'
#' @return The shaid of the saved blob
#'
#' @export
ngchmSaveAsDatasetBlob <- function (shaidyRepo, format, mat) {
    stopifnot (format == 'tsv');
    filename <- tempfile ("matrix", fileext='.tsv');
    write.table (mat, filename, quote=FALSE, sep='\t');
    shaid <- ngchmAddDatasetBlob (shaidyRepo$blob.path, format, filename);
    unlink (filename);
    shaid
}

#' Load a data matrix from a local shaidy repository
#'
#' @param shaidyRepo The shaidy repository
#' @param shaid The shaid of the dataset blob to load
#'
#' @return a list containing details of the loaded dataset
#'
#' @import jsonlite
#'
#' @export
ngchmLoadDatasetBlob <- function (shaidyRepo, shaid) {
    blobdir <- shaidyRepo$blob.path ('dataset', shaid@value);
    props <- jsonlite::fromJSON (readLines (file.path (blobdir, "properties.json")));
    mat <- tsvio::tsvGetData (file.path (blobdir, "matrix.tsv"), file.path (blobdir, "index.tsv"),
                              NULL, NULL, 0.0);
    list (shaid=shaid, properties=props, mat=mat)
}

#' Row center a shaidy dataset
#'
#' @param shaidyRepo The shaidy repository
#' @param shaid The shaid of the dataset to row center
#'
#' @return A list of shaids for the row centered dataset
ngchmRowCenter <- function (shaidyRepo, shaid) {
    provid <- shaidyProvenance (shaidyRepo, name="ngchmRowCenter", shaid=shaid@value);
    res <- shaidyRepo$provenanceDB$get (provid);
    if (length(res) == 0) {
        ds <- ngchmLoadDatasetBlob (shaidyRepo, shaid);
	mm <- rowMeans (ds$mat, na.rm=TRUE);
	res <- list(ngchmSaveAsDatasetBlob (shaidyRepo, 'tsv', ds$mat-mm));
	shaidyRepo$provenanceDB$insert (provid, res[[1]]);
    }
    res
}

