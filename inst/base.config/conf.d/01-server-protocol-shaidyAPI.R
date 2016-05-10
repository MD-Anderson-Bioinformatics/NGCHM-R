# Install a shaidy format NGCHM in a remote shaidyAPI repository.
(function() {

  validator <- function (params) {
  };

  ngchmSaveBlob <- function (obj, blob.path) {
      return (FALSE);
  };

ngchmCreateServerProtocol ("shaidyapi",
    requiredParams = c('baseurl'),
    optionalParams = NULL,
    paramValidator = validator,
    installMethod = function (server, chm, collectionId) {
	stopifnot (chm@format == "shaidy");
        stopifnot (!missing(collectionId));
        shaid <- shaidyGetShaid (chm);
	shaidyURL <- ngchmGetProtoParam (server, 'baseurl');
        shaidyRepo <- shaidyLoadRepository ('api', shaidyURL);
        tocheck <- c(shaid, shaidyGetComponents(chm));
        present <- shaidyBlobExists (shaidyRepo, tocheck);
        for (sid in tocheck[!present]) {
	    cat (sprintf ("Copying blob %s/%s to repository\n", sid@type, sid@value), file=stderr());
            repo <- ngchmFindRepo (sid);
            stopifnot (length(repo) > 0);
            shaidyCopyBlob (repo, sid, shaidyRepo);
        }
	cat (sprintf ("Saving chm %s to %s\n", chm@name, if(collectionId=="") "base collection" else sprintf ("collection %s",collectionId)), file=stderr());
        collection <- ngchmLoadCollection (shaidyRepo, collectionId);
	ngchmAddObjectToCollection (shaidyRepo, collection, shaid);
	return (invisible(shaid));
	},
    uninstallMethod = function (server, chmname, collectionIds) {
	shaidyURL <- ngchmGetProtoParam (server, 'baseurl');
        shaidyRepo <- shaidyLoadRepository ('api', shaidyURL);
	stop ("Not yet implemented by shaidyapi protocol");
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
