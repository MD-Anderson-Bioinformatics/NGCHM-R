# Define server protocol for "Simple CHM Loader" (SCL).
#
(function() {

    if (Sys.info()[['sysname']] != "Windows")  {
        thisHost <- c("localhost", system("/bin/hostname -f", intern=TRUE));
    } else {
        thisHost <- c("localhost", paste(Sys.getenv('COMPUTERNAME'),Sys.getenv('USERDNSDOMAIN'),sep='.'));
    }

    chmDeployDir <- function(server) ngchmGetProtoParam (server, 'deployDir');

    getServerDest <- function(server) {
        if (!"ngchmServer" %in% class(server)) server <- chmServer(server);
	dest <- server@deployServer;
	username <- ngchmGetProtoParam (server, 'username');
	keypath <- ngchmGetProtoParam (server, 'keypath');
	if (!is.null(username)) { dest <- sprintf("%s@%s", username, dest); }
	dest <- shQuote (dest);
	if (!is.null(keypath)) { dest <- sprintf("-i %s %s", shQuote(keypath), dest); }
	dest
    };

    systemCheck <- function (cmd, ...) {
        res <- system (cmd, ...);
	if (res != 0) stop (sprintf ('System command "%s" failed', cmd));
	invisible(res)
    };

    validator <- function (params) {
    };

ngchmCreateServerProtocol ("scl",
    requiredParams = c('deployDir'),
    optionalParams = c('username','keypath'),
    paramValidator = validator,

     installMethod = function (server, chm) {
	# Install the NGCHM using the SCL system.
	# tar lives in different places on different systems - so no absolute path
	tarballName <- sprintf ("%s.ngchm.gz", chm@name);
	ddir <- chmDeployDir (server);
	if (server@deployServer %in% thisHost) {
	    systemCheck (sprintf ("tar xfC %s %s/STAGE", shQuote(tarballName), shQuote(ddir)));
	    systemCheck (sprintf ("chmod -R ugo+w %s/STAGE/%s", shQuote(ddir), shQuote(chm@name)));
	    systemCheck (sprintf ("/bin/mv %s/STAGE/%s %s/ADD/", shQuote(ddir), shQuote(chm@name), shQuote(ddir)));
	    systemCheck (sprintf ("while [ -d %s/ADD/%s ] ; do sleep 1; done", shQuote(ddir), shQuote(chm@name)));
	} else {
	    dest <- getServerDest (server);
	    systemCheck (sprintf ("scp %s %s:%s/STAGE/", shQuote(tarballName), dest, shQuote (chmDeployDir(server))));
	    systemCheck (sprintf ("ssh %s tar xfC %s/STAGE/%s %s/STAGE", dest, shQuote(ddir), shQuote(tarballName), shQuote(ddir)));
	    systemCheck (sprintf ("ssh %s /bin/rm %s/STAGE/%s", dest, shQuote(ddir), shQuote(tarballName)));
	    systemCheck (sprintf ("ssh %s /bin/mv %s/STAGE/%s %s/ADD/", dest, shQuote(ddir), shQuote(chm@name), shQuote(ddir)));
	    systemCheck (sprintf ("ssh %s while [ -d %s/ADD/%s ] ';' do sleep 1 ';' done", dest, shQuote(ddir), shQuote(chm@name)));
	}
     },
     uninstallMethod = function (server, chmname) {
	# Uninstall the NGCHM using the SCL system.
	ddir <- chmDeployDir (server);
	if (server@deployServer %in% thisHost) {
	    systemCheck (sprintf ("/bin/mkdir %s/STAGE/%s", shQuote(ddir), shQuote(chmname)));
	    systemCheck (sprintf ("chmod ugo+w %s/STAGE/%s", shQuote(ddir), shQuote(chmname)));
	    systemCheck (sprintf ("/bin/mv %s/STAGE/%s %s/REMOVE/", shQuote(ddir), shQuote(chmname), shQuote(ddir)));
	    systemCheck (sprintf ("while [ -d %s/REMOVE/%s ] ; do sleep 1; done", shQuote(ddir), shQuote(chmname)));
	} else {
	    dest <- getServerDest (server);
	    systemCheck (sprintf ("ssh %s /bin/mkdir %s/REMOVE/%s", dest, shQuote(ddir), shQuote(chmname)));
	    systemCheck (sprintf ("ssh %s while [ -d '%s'/REMOVE/'%s' ] ';' do sleep 1 ';' done", dest, shQuote(ddir), shQuote(chmname)));
	}
     },
     makePrivate = function (server, chmname) {
	# Make an NGCHM private (add hidden.txt file) using the SCL system.
	ddir <- chmDeployDir (server);
	if (server@deployServer %in% thisHost) {
	    systemCheck (sprintf ("/bin/mkdir %s/STAGE/%s", shQuote(ddir), shQuote(chmname)));
	    systemCheck (sprintf ("/bin/touch %s/STAGE/%s/hidden.txt", shQuote(ddir), shQuote(chmname)));
	    systemCheck (sprintf ("chmod -R ugo+w %s/STAGE/%s", shQuote(ddir), shQuote(chmname)));
	    systemCheck (sprintf ("/bin/mv %s/STAGE/%s %s/ADD-FILE/", shQuote(ddir), shQuote(chmname), shQuote(ddir)));
	    systemCheck (sprintf ("while [ -d %s/ADD-FILE/%s ] ; do sleep 1; done", shQuote(ddir), shQuote(chmname)));
	} else {
	    dest <- getServerDest (server);
	    systemCheck (sprintf ("ssh %s /bin/mkdir %s/STAGE/%s", dest, shQuote(ddir), shQuote(chmname)));
	    systemCheck (sprintf ("ssh %s /bin/touch %s/STAGE/%s/hidden.txt", dest, shQuote(ddir), shQuote(chmname)));
	    systemCheck (sprintf ("ssh %s /bin/mv %s/STAGE/%s %s/ADD-FILE/", dest, shQuote(ddir), shQuote(chmname), shQuote(ddir)));
	    systemCheck (sprintf ("ssh %s while [ -d %s/ADD-FILE/%s ] ';' do sleep 1 ';' done", dest, shQuote(ddir), shQuote(chmname)));
	}
     },
     makePublic = function (server, chmname) {
	# Make an NGCHM public (remove hidden.txt file) using the SCL system.
	ddir <- chmDeployDir (server);
	if (server@deployServer %in% thisHost) {
	    systemCheck (sprintf ("/bin/mkdir %s/STAGE/%s", shQuote(ddir), shQuote(chmname)));
	    systemCheck (sprintf ("/bin/touch %s/STAGE/%s/hidden.txt", shQuote(ddir), shQuote(chmname)));
	    systemCheck (sprintf ("chmod -R ugo+w %s/STAGE/%s", shQuote(ddir), shQuote(chmname)));
	    systemCheck (sprintf ("/bin/mv %s/STAGE/%s %s/REMOVE-FILE/", shQuote(ddir), shQuote(chmname), shQuote(ddir)));
	    systemCheck (sprintf ("while [ -d %s/REMOVE-FILE/%s ] ; do sleep 1; done", shQuote(ddir), shQuote(chmname)));
	} else {
	    dest <- getServerDest (server);
	    systemCheck (sprintf ("ssh %s /bin/mkdir %s/STAGE/%s", dest, shQuote(ddir), shQuote(chmname)));
	    systemCheck (sprintf ("ssh %s /bin/touch %s/STAGE/%s/hidden.txt", dest, shQuote(ddir), shQuote(chmname)));
	    systemCheck (sprintf ("ssh %s /bin/mv %s/STAGE/%s %s/REMOVE-FILE/", dest, shQuote(ddir), shQuote(chmname), shQuote(ddir)));
	    systemCheck (sprintf ("ssh %s while [ -d %s/REMOVE-FILE/%s ] ';' do sleep 1 ';' done", dest, shQuote(ddir), shQuote(chmname)));
	}
     }
);

})();
