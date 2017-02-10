# Install a shaidy format NGCHM in a local shaidydir repository.
(function() {

    findCollection <- function (repo, collection, parts) {
        while (length(parts) > 0 && parts[1]=="") {
            parts <- parts[-1];
        }
	if (length(parts)==0) {
	    collection$uuid
	} else {
            fields <- strsplit (parts[1], '=')[[1]];
            if (length(fields) == 1) {
                fields <- c("name", fields);
            } else if (length(fields) > 2) {
                fields <- c(fields[1], paste(fields[-1], collapse='='));
            }
	    result <- c();
	    for (uuid in collection$collections) {
		shaid <- new ('shaid', type='collection', value=uuid);
		labels <- repo$loadProperty(shaid, "labels");
                if (any ((labels$Name==fields[1])&(labels$Value==fields[2]))) {
		    if (length(parts) == 1) {
			# Avoid unnecessary LoadCollection if this is last part.
			result <- c (result, uuid);
		    } else {
			result <- c (result, findCollection (repo, repo$loadCollection(uuid), parts[-1]));
		    }
                }
	    }
	    return (result);
	}
    };

    createCollection <- function (repo, collection, name) {
        stopifnot (typeof(name)=="character" && length(name) == 1 && name != "");
	fields <- strsplit (name, '=')[[1]];
	if (length(fields) == 1) {
	    fields <- c("name", fields);
	} else if (length(fields) > 2) {
	    fields <- c(fields[1], paste(fields[-1], collapse='='));
	}
        newCollection <- ngchmNewCollection (repo, labels=data.frame(Name=fields[1], Value=fields[2]));
        repo$addCollectionToCollection (collection, newCollection);
        invisible (newCollection)
    };

    getServerRepo <- function (server) {
	accessMethod <- ngchmGetProtoParam (server, 'accessMethod');
	shaidyBase <- ngchmGetProtoParam (server, 'basePath');
        shaidyLoadRepository (accessMethod, shaidyBase)
    };

ngchmCreateServerProtocol ("shaidy",
    chmFormat = "shaidy",
    requiredParams = c('accessMethod','basePath'),
    findCollection = function (server, collectionId, parts) {
        shaidyRepo <- getServerRepo (server);
	collection <- shaidyRepo$loadCollection(collectionId);
	findCollection (shaidyRepo, collection, parts)
    },
    createCollection = function (server, collectionId, name) {
        shaidyRepo <- getServerRepo (server);
	collection <- shaidyRepo$loadCollection(collectionId);
        createCollection (shaidyRepo, collection, name)
    },
    installMethod = function (server, chm, collection) {
    	stopifnot (chm@format == "shaidy");
        shaidyRepo <- getServerRepo (server);
        shaid <- shaidyGetShaid (chm);
        tocheck <- c(shaid, shaidyGetComponents(chm));
        present <- shaidyBlobExists (shaidyRepo, tocheck);
        for (sid in tocheck[!present]) {
	    cat (sprintf ("Copying blob %s/%s to repository\n", sid@type, sid@value), file=stderr());
            repo <- ngchmFindRepo (sid);
            stopifnot (length(repo) > 0);
            shaidyCopyBlob (repo, sid, shaidyRepo);
        }
#        if (NGCHM:::testExternalProgram('tiledata')) {
#            tiles <- mapply(function(x) {
#                ngchmTileDataset(shaidyRepo,x@data,chm@rowOrder,chm@colOrder)[[1]]
#            }, chm@layers, SIMPLIFY=FALSE);
#            tileFile <- shaidyRepo$blob.path(shaid,"tiles.json");
#            if (!file.exists(tileFile)) {
#	        writeLines(jsonlite::toJSON(tiles), tileFile);
#	    }
#        }
	for (uuid in collection) {
	    cat (sprintf ("Saving chm %s to %s\n", chm@name, uuid), file=stderr());
	    ngchmAddObjectToCollection (shaidyRepo, uuid, shaid);
	}
        ngchmRenderChm (shaidyRepo, shaid);
	return (invisible(shaid));
    },
    uninstallMethod = function (server, chmname, collectionIds) {
        shaidyRepo <- getServerRepo (server);
	stop ("Not yet implemented by shaidydir protocol");
	return (invisible(FALSE));
    }
);
})();
