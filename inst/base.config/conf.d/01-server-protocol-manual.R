
# Define the built-in server protocol "manual".
#
(function() {

ngchmCreateServerProtocol ("manual",
    requiredParams = NULL,
    optionalParams = NULL,
    paramValidator = function (params) {
    },
    findCollection = function (server, collectionId, path) {
        return (NULL);
    },
    installMethod = function (server, chm) {
	stop ("NGCHMs cannot be automatically installed on this server. Please obtain installation instructions from the server administrator.");
    },
    uninstallMethod = function (server, chm) {
	stop ("NGCHMs cannot be automatically uninstalled from this server. Please obtain instructions from the server administrator.");
    },
    makePrivate = function (server, chm) {
	stop ("NGCHMs cannot be automatically be made private on this server. Please obtain instructions from the server administrator.");
    },
    makePublic = function (server, chm) {
	stop ("NGCHMs cannot be automatically be made public on this server. Please obtain instructions from the server administrator.");
    }
);

})();
