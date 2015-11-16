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
	if (!shaid %in% old) {
	    assign (provid, append (old, shaid), envir=db, inherits=FALSE);
	}
    };

    ggg <- function (provids) {
	for (pp in provids) {
	    ids <- get0 (pp, envir=db, inherits=FALSE);
	    if (length(ids)>0) return (ids);
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
#' @param repo The shaidy repository to print
#'
#' @return The shaidy repository
#'
#' @export
print.shaidyRepo <- function (repo) {
    cat ("Shaidy repository at", repo$basepath, "\n");
    invisible (repo)
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

#' Create a provid from a list of label values
#'
#' @param shaidyRepo The shaidy repository in which to store the provid -> labels data
#' @param ... A list of name=value labels to store in the provid
#'
#' @return A string containing the provid for the list of label values.
#'
#' @export
shaidyProvenance <- function (shaidyRepo, ...) {
    labels <- list (...);
    oo <- order (names (labels));
    provid <- gitSha (paste(sprintf ("%s=%s", names(labels)[oo], labels[oo]), collapse=','));
    shaidyRepo$providDB$assign (provid, labels);
    provid
}

