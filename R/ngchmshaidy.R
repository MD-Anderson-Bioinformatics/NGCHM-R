#' Create a shaidy repository for NG-CHMS
#'
#' @param shaidyDir Basepath of local shaidy repository to create
#'
#' @export
ngchmInitShaidyRepository <- function (shaidyDir) {
    shaidyInitRepository (shaidyDir, c("collection", "chm", "dataset", "dendrogram", "label", "tile", "viewer"))
}

#' Push a shaidy repository onto the stack of temporary repositories
#'
#' @param shaidyDir Basepath of local shaidy repository to use as a temporary repository
#'
#' @export
ngchmPushTempRepository <- function (shaidyDir) {
    newrepo <- shaidyLoadRepository (shaidyDir);
    ngchm.env$tmpShaidyStack <- c(list(ngchm.env$tmpShaidy), ngchm.env$tmpShaidyStack);
    ngchm.env$tmpShaidy <- newrepo
}

#' Push a shaidy repository onto the stack of source repositories
#'
#' @param shaidyDir Basepath of local shaidy repository to use as a source repository
#'
#' @export
ngchmPushSourceRepository <- function (shaidyDir) {
    newrepo <- shaidyLoadRepository (shaidyDir);
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
    stopifnot (is(shaid,"shaid"),
               shaid@type == 'chm');
    if (!shaid@value %in% collection$chms) {
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
    shaid <- ngchmGetDataFileShaid (format, filename);
    shaidyAddFileBlob (shaidyRepo, 'dataset', 'matrix.tsv', filename,
                       properties=c(list(format=format),properties), shaid=shaid);
    shaid
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
	       is.numeric (mat),
	       length (dim(mat)) == 2,
	       length (rownames(mat)) > 0,
	       length (colnames(mat)) > 0);
    filename <- utempfile ("matrix", fileext='.tsv');
    write.table (mat, filename, quote=FALSE, sep='\t');
    shaid <- ngchmAddDatasetBlob (shaidyRepo, format, filename,
                                  list(nrow=nrow(mat),ncol=ncol(mat)));
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
    shaid <- shaidyAddFileBlob (shaidyRepo, 'dendrogram',
                                c('dendrogram-data.tsv', 'dendrogram-order.tsv', 'dendrogram.str'),
                                c(datafilename, orderfilename, ddgfilename));
    unlink (datafilename);
    unlink (orderfilename);
    unlink (ddgfilename);
    shaid
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
        if (shaid@type == 'dataset') {
            ds <- ngchmLoadDatasetBlob (shaidyRepo, shaid);
            labels <- (if (axis=="row") rownames else colnames)(ds$mat);
        } else if (shaid@type == 'dendrogram') {
	    oo <- read.delim (shaidyRepo$blob.path(shaid@type,shaid@value,"dendrogram-order.tsv"), header=TRUE, colClasses=c('character','numeric'));
	    labels <- oo$Id[order(oo$Order)]
        } else {
	    stop (sprintf ("Unknown shaid type %s", shaid@type));
        }
	filename <- utempfile ("label", fileext='.txt');
	writeLines (labels, filename);
	res <- list(shaidyAddFileBlob (shaidyRepo, 'label', 'labels.txt', filename));
        unlink (filename);
	shaidyRepo$provenanceDB$insert (provid, res[[1]]);
    }
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
