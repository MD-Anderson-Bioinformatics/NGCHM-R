#requireNameSpace(jsonlite)
requireNamespace('httr')

# Interact with an NGCHM server using the NGCHM Manager protocol.
#
(function() {
handledb <- new.env (hash=TRUE, parent=emptyenv());

h <- function (server) {
    if (!exists (server@deployServer, handledb)) {
        assign (server@deployServer, httr::handle(server@deployServer), handledb);
    }
    get (server@deployServer, handledb)
}

chmCreateServerProtocol ("manager",
    installMethod = function (server, chm) {
	chmFileName <- sprintf ("%s.ngchm.gz", chm@name);
	stopifnot (file.exists (chmFileName));
	res <- httr::POST(url = sprintf ("%s/put", server@deployServer),
	            body = list (server=server@deployDir, name=chm@name, limitAction='none', file=httr::upload_file (chmFileName)),
	            encode = "multipart", c(chmGetDeployServerConfig(server),httr::progress("up")), handle=h(server));
	cat("\n");
	if ((res$status_code < 200) || (res$status_code >= 300)) {
		cat(rawToChar(res$content), "\n")
	}
	return (invisible(res));
    },
    uninstallMethod = function (server, chmname) {
	res <- httr::DELETE(url = sprintf ("%s/delete", server@deployServer),
                      chmGetDeployServerConfig(server),
	              params=sprintf("?server=%s&name=%s", server@deployDir, chmname),
                      handle=h(server));
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
