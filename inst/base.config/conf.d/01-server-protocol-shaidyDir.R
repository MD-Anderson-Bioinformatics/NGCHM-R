# Install a shaidy format NGCHM in a local shaidydir repository.
(function() {

  validator <- function (params) {
  };

  ngchmSaveBlob <- function (obj, blob.path) {
      return (FALSE);
  };

ngchmCreateServerProtocol ("shaidydir",
    requiredParams = c('basepath'),
    optionalParams = NULL,
    paramValidator = validator,
    installMethod = function (server, chm, collectionId) {
	stopifnot (chm@format == "shaidy");
        shaid <- shaidyGetShaid (chm);
	shaidyDir <- ngchmGetProtoParam (server, 'basepath');
        shaidyRepo <- shaidyLoadRepository ('file', shaidyDir);
        collection <- ngchmLoadCollection (shaidyRepo, collectionId);
        tocheck <- c(shaid, shaidyGetComponents(chm));
        present <- shaidyBlobExists (shaidyRepo, tocheck);
        for (sid in tocheck[!present]) {
	    cat (sprintf ("Copying blob %s/%s to repository\n", sid@type, sid@value), file=stderr());
            repo <- ngchmFindRepo (sid);
            stopifnot (length(repo) > 0);
            shaidyCopyBlob (repo, sid, shaidyRepo);
        }
        if (NGCHM:::testExternalProgram('tiledata')) {
            tiles <- mapply(function(x) {
                ngchmTileDataset(shaidyRepo,x@data,chm@rowOrder,chm@colOrder)[[1]]
            }, chm@layers, SIMPLIFY=FALSE);
            tileFile <- shaidyRepo$blob.path(shaid,"tiles.json");
            if (!file.exists(tileFile)) {
	        writeLines(jsonlite::toJSON(tiles), tileFile);
	    }
        }
	cat (sprintf ("Saving chm %s to %s\n", chm@name, if(collectionId=="") "base collection" else sprintf ("collection %s",collectionId)), file=stderr());
	ngchmAddChmToCollection (collection, shaid);
	return (invisible(shaid));
	},
    uninstallMethod = function (server, chmname, collectionIds) {
	shaidyDir <- ngchmGetProtoParam (server, 'basepath');
        shaidyRepo <- shaidyLoadRepository ('file', shaidyDir);
	stop ("Not yet implemented by shaidydir protocol");
	return (invisible(FALSE));
    },
    makePrivate = function (server, chmname) {
	stop ("Not yet implemented by shaidydir protocol");
    },
    makePublic = function (server, chmname) {
	stop ("Not yet implemented by shaidydir protocol");
    }
);
})();
