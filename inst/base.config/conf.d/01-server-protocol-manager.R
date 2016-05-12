requireNamespace('httr')

# Interact with an NGCHM server using the NGCHM Manager protocol.
#
(function() {

  validator <- function (params) {
  };

  configAndProgress <- function (server) {
    cfg <- chmGetDeployServerConfig (server);
    prg <- httr::progress("up");
    if (is.null(cfg)) prg else c(cfg,prg)
  }

ngchmCreateServerProtocol ("manager",
    requiredParams = c('serviceName'),
    optionalParams = NULL,
    paramValidator = validator,
    findCollection = function (server, collectionId, path) {
        return (NULL);
    },
    installMethod = function (server, chm) {
	chmFileName <- sprintf ("%s.ngchm.gz", chm@name);
	stopifnot (file.exists (chmFileName));
	serviceName <- ngchmGetProtoParam (server, 'serviceName');
	res <- httr::POST(url = sprintf ("%s/put", server@deployServer),
	            body = list (server=serviceName, name=chm@name, limitAction='none', file=httr::upload_file (chmFileName)),
	            encode = "multipart", configAndProgress(server), handle=ngchmGetHandleHTTR(server));
	cat("\n");
	if ((res$status_code < 200) || (res$status_code >= 300)) {
		cat(rawToChar(res$content), "\n")
	}
	return (invisible(res));
    },
    uninstallMethod = function (server, chmname) {
	serviceName <- ngchmGetProtoParam (server, 'serviceName');
	res <- httr::DELETE(url = sprintf ("%s/delete", server@deployServer),
                      chmGetDeployServerConfig(server),
	              params=sprintf("?server=%s&name=%s", serviceName, chmname),
                      handle=ngchmGetHandleHTTR(server));
	if ((res$status_code < 200) || (res$status_code >= 300)) {
		cat(rawToChar(res$content), "\n")
	}
	return (invisible(res));
    },
    makePrivate = function (server, chmname) {
	stop ("Not yet implemented by NGCHM Manager protocol");
    },
    makePublic = function (server, chmname) {
	stop ("Not yet implemented by NGCHM Manager protocol");
    }
);
})();
