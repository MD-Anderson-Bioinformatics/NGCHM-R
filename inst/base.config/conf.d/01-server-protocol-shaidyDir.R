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
        shaid <- ngchmGetShaid (chm);
	shaidyDir <- ngchmGetProtoParam (server, 'basepath');
        shaidyRepo <- shaidyLoadRepository (shaidyDir);
        collection <- ngchmLoadCollection (shaidyRepo, collectionID);
	cat (sprintf ("Saving chm %s to collection %s\n", chm@name, collectionId), file=stderr());
	ngchmAddChmToCollection (collection, shaid);
	return (invisible(shaid));
	},
    uninstallMethod = function (server, chmname, collectionIds) {
	shaidyDir <- ngchmGetProtoParam (server, 'basepath');
        shaidyRepo <- shaidyLoadRepository (shaidyDir);
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
