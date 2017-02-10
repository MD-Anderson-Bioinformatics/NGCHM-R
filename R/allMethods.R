# ##############################################################################################
#
# Methods for class NGCHM.SERVER
#

chmServerCheck <- function (name) {
    server <- chmServer (name);
    if (length(server) == 0) {
        stop (sprintf ("Unknown CHM server '%s'", name));
    }
    server
}

#' @rdname chmDeployServer-method
#' @aliases chmDeployServer,ngchmServer-method
#'
setMethod ("chmDeployServer",
    signature = c(server="ngchmServer"),
    definition = function (server) server@deployServer);

#' @rdname chmUrlBase-method
#' @aliases chmUrlBase,ngchmServer-method
#'
setMethod ("chmUrlBase",
    signature = c(server="ngchmServer"),
    definition = function (server) sprintf ("%s/chm.html", server@serverURL));


#' @rdname chmInstall-method
#' @aliases chmInstall,ngchm-method
setMethod ("chmInstall",
    signature = c(chm="ngchm"),
    definition = function (chm, path, ...) {
        if (missing(path)) {
            dest <- list (server=chmCurrentServer(), collection=chmCurrentCollection());
        } else {
            dest <- parsePathSpec (path);
        }
        if (typeof(dest$server)=="character") dest$server <- chmServerCheck(dest$server);
        stopifnot(length(dest$server) > 0);

        maker <- get (sprintf ("ngchmMakeFormat.%s", dest$server@serverProtocol@chmFormat));
        installer <- dest$server@serverProtocol@installMethod;

        args <- list(...);
        make.args <- list();
        install.args <- list();
        if ("server" %in% names(formals(maker))) {
            make.args <- list(server=dest$server);
        }
        if ("collection" %in% names(formals(installer))) {
            install.args <- list(collection=dest$collection);
        }
        if (length(args) > 0) {
            stopifnot (!is.null(names(args)));
            for (ii in 1:length(args)) {
                if (names(args)[ii] %in% names(formals(maker))) {
                    make.args <- c (make.args, args[[ii]]);
                } else if (names(args)[ii] %in% names(formals(maker))) {
                    install.args <- c (install.args, args[[ii]]);
                } else {
                    stop ("unknown parameter ", names(args)[ii]);
                }
            }
        }

	chm <- chmAddProperty (chm, "chm.info.build.time", format(Sys.time(), "%F %H:%M:%S"));

        chm <- chmMake (chm);
        chm@format <- dest$server@serverProtocol@chmFormat;
        chm <- do.call (maker, c(chm, make.args));
    	do.call (installer, c(dest$server, chm, install.args));
        chm
    });

### ' @rdname chmInstall-method
### ' @aliases chmInstall,character,ngchm-method
##setMethod ("chmInstall",
##    signature = c(server="character", chm="ngchm"),
##    definition = function (server, chm, ...) {
##	chmInstall (chmServerCheck (server), chm, ...);
##    });

#' @rdname chmUninstall-method
#' @aliases chmUninstall,character-method
setMethod ("chmUninstall",
    signature = c(chm="character"),
    definition = function (chm, server=NULL, ...) {
        if (length(server)==0) server <- chmCurrentServer();
        stopifnot(length(server) > 0);
        if (typeof(server) == 'character') server <- chmServerCheck (server);
	server@serverProtocol@uninstallMethod (server, chm, ...);
    });

### ' @rdname chmUninstall-method
### ' @aliases chmUninstall,ngchmServer,ngchm-method
##setMethod ("chmUninstall",
##    signature = c(server="ngchmServer", chm="ngchm"),
##    definition = function (server, chm, ...) {
##        chmUninstall (server, chmName(chm), ...);
##});

#' @rdname chmUninstall-method
#' @aliases chmUninstall,ngchm-method
setMethod ("chmUninstall",
    signature = c(chm="ngchm"),
    definition = function (chm, ...) {
        chmUninstall (chmName(chm), ...);
});

### ' @rdname chmUninstall-method
### ' @aliases chmUninstall,character,character-method
##setMethod ("chmUninstall",
##    signature = c(server="character", chm="character"),
##    definition = function (server, chm, ...) {
##        chmUninstall (chmServerCheck(server), chm, ...);
##    });

#' @rdname chmMakePrivate-method
#' @aliases chmMakePrivate,ngchmServer,character-method
setMethod ("chmMakePrivate",
    signature = c(server="ngchmServer", chm="character"),
    definition = function (server, chm) {
	server@serverProtocol@makePrivate (server, chm);
    });

#' @rdname chmMakePrivate-method
#' @aliases chmMakePrivate,ngchmServer,ngchm-method
setMethod ("chmMakePrivate",
    signature = c(server="ngchmServer", chm="ngchm"),
    definition = function (server, chm) {
        chmMakePrivate (server, chmName(chm));
});

#' @rdname chmMakePrivate-method
#' @aliases chmMakePrivate,character,ngchm-method
setMethod ("chmMakePrivate",
    signature = c(server="character", chm="ngchm"),
    definition = function (server, chm) {
        chmMakePrivate (chmServerCheck(server), chmName(chm));
});

#' @rdname chmMakePrivate-method
#' @aliases chmMakePrivate,character,character-method
setMethod ("chmMakePrivate",
    signature = c(server="character", chm="character"),
    definition = function (server, chm) {
        chmMakePrivate (chmServerCheck(server), chm);
    });

#' @rdname chmMakePublic-method
#' @aliases chmMakePublic,ngchmServer,character-method
setMethod ("chmMakePublic",
    signature = c(server="ngchmServer", chm="character"),
    definition = function (server, chm) {
	server@serverProtocol@makePublic (server, chm);
    });

#' @rdname chmMakePublic-method
#' @aliases chmMakePublic,ngchmServer,ngchm-method
setMethod ("chmMakePublic",
    signature = c(server="ngchmServer", chm="ngchm"),
    definition = function (server, chm) {
        chmMakePublic (server, chmName(chm));
});

#' @rdname chmMakePublic-method
#' @aliases chmMakePublic,character,ngchm-method
setMethod ("chmMakePublic",
    signature = c(server="character", chm="ngchm"),
    definition = function (server, chm) {
        chmMakePublic (chmServerCheck(server), chmName(chm));
});

#' @rdname chmMakePublic-method
#' @aliases chmMakePublic,character,character-method
setMethod ("chmMakePublic",
    signature = c(server="character", chm="character"),
    definition = function (server, chm) {
        chmMakePublic (chmServerCheck(server), chm);
    });


#' @rdname chmLoadCHM-method
#' @aliases chmLoadCHM,ngchmServer,character-method
setMethod ("chmLoadCHM",
    signature = c(serverOrURL="ngchmServer",name="character"),
    definition = function (serverOrURL, name) {
	loadChmFromURL (chmGetURL (name, server=serverOrURL))
    });

#' @rdname chmLoadCHM-method
#' @aliases chmLoadCHM,character,character-method
setMethod ("chmLoadCHM",
    signature = c(serverOrURL="character",name="character"),
    definition = function (serverOrURL, name) {
	if (serverOrURL %in% chmListServers()) {
	    loadChmFromURL (chmGetURL (name, server=serverOrURL))
	} else {
	    stop (sprintf ("Unknown server '%s'", serverOrURL));
	}
    });

#' @rdname chmLoadCHM-method
#' @aliases chmLoadCHM,character,missing-method
setMethod ("chmLoadCHM",
    signature = c(serverOrURL="character",name="missing"),
    definition = function (serverOrURL, name) {
        loadChmFromURL (serverOrURL)
    });

# ##############################################################################################
#
# Methods for class NGCHM
#

loadChmFromURL <- function (chmurl) {
    params <- strsplit (chmurl, "?", fixed=TRUE)[[1]];
    if (substring (params[1], nchar(params[1])-8) != "/chm.html") {
        stop (sprintf ("url '%s' does not look like an NG-CHM url", chmurl));
    } else {
        baseurl <- substr (params[1], 1, nchar(params[1])-8);
    }
    params <- strsplit(params[2:length(params)],'=');
    idx <- which (vapply (params, function(x)x[1]=="name", TRUE));
    if (length (idx) != 1) {
        stop (sprintf ("url '%s' does not look like an NG-CHM url", chmurl));
    }
    chmname <- params[[idx]][2];
    ee <- new.env();
    load(url(paste (baseurl, "data/", chmname, "/undefined/chm.Rdata", sep="")), ee);
    chm <- chmFixVersion (ee$chm);
    chm@inpDir <- utempfile ("ngchm.input");
    chm@outDir <- utempfile ("ngchm.output");
    chm@saveDir <- ".";
    try (ngchmPushSourceRepository(paste(baseurl, "data/", chmname, '/undefined/shaidyRepo.tar', sep=""),'http'),TRUE);
    chm
}

writeColorMap <- function (context, cmap, prefix, suffix, chan) {
    stopifnot (length(cmap@missing) > 0);
    colorstr = c("[");
    thresstr = c("[");
    if (is.list(cmap@points)) {
	for (ii in 1:length(cmap@points)) {
	    if (ii > 1) {
		colorstr = append(colorstr,";");
		thresstr = append(thresstr,";");
	    }
	    colorstr = append(colorstr, cmap@points[[ii]]@color);
	    thresstr = append(thresstr, cmap@points[[ii]]@value);
	}
    }
    colorstr = append(colorstr,"]");
    thresstr = append(thresstr,"]");
    cat (sprintf ("%s.color.type%s=%s\n", prefix, suffix, cmap@type), file=chan);
    cat (sprintf ("%s.missing.color%s=%s\n", prefix, suffix, cmap@missing), file=chan);
    cat (sprintf ("%s.colors%s=%s\n", prefix, suffix, paste(colorstr,collapse="")), file=chan);
    if (context == "class") {
	cat (sprintf ("%s.values%s=%s\n", prefix, suffix, paste(thresstr,collapse="")), file=chan);
    } else {
	cat (sprintf ("%s.thresholds%s=%s\n", prefix, suffix, paste(thresstr,collapse="")), file=chan);
    }
}

jsonColorMap <- function (context, cmap) {
    stopifnot (length(cmap@missing) > 0);
    list (type=cmap@type,
          missing=cmap@missing,
	  colors=vapply(cmap@points, function(p)p@color, ""),
	  values=vapply(cmap@points, function(p)as.character(p@value), ""))
}

writeMenu <- function (menu, prefix, chan) {
    if (is.list(menu)) {
	for (ii in 1:length(menu)) {
	    cat (sprintf ("    chm.%s.addMenuItem ('%s', %s)\n", prefix, menu[[ii]]@label, menu[[ii]]@fun), file=chan);
	}
    }
}

writeCSS <- function (css, inpDir) {
    chan <- file (file.path (inpDir, "custom.css"), "w");
    for (ii in 1:length(css))
        cat (css[[ii]]@css, sep="\n", file=chan);
    close (chan);
}

hasSpecialProperties <- function (chm) {
    any (vapply (chm@properties, function (p) substr(p@label,1,1)=='!', TRUE))
}

writeProperties <- function (inpDir, format, props, chan, writeSpecial=FALSE) {
    if (writeSpecial) {
	for (ii in 1:length(props)) {
	    l <- props[[ii]]@label;
	    if (substr(l,1,1) == '!') {
		cat (sprintf ("%s=%s", substring(l,2), props[[ii]]@value), sep="\n", file=chan);
	    }
	}
    }
    else {
	for (ii in 1:length(props)) {
	    l <- props[[ii]]@label;
	    if (substr(l,1,1) != '!') {
		if (l != "hidden" && l != "hidden.tags") {
		    cat (sprintf ("%s=%s", l, props[[ii]]@value), sep="\n", file=chan);
		}
	    }
	}
    }
}

writePropertiesPost <- function (outDir, format, props) {
    hidden.tags <- NULL;
    for (ii in 1:length(props)) {
	if (props[[ii]]@label == "hidden.tags") {
	    hidden.tags <- sprintf ("%s\n", props[[ii]]@value);
	}
    }
    for (ii in 1:length(props)) {
	if ((props[[ii]]@label == "hidden") && (props[[ii]]@value == "TRUE")) {
	    if (format == "original") {
	        cat (hidden.tags, sep='', file=file.path (outDir, "hidden.txt"));
	    } else {
	        hidden.tags <- sub("\n","",hidden.tags);
                writeLines(jsonlite::toJSON(hidden.tags,pretty=TRUE), file.path (outDir, "hidden.json"));
	    }
	}
    }
}

writeChmPost <- function (chm, outdir=NULL) {
    if (length(outdir)==0) outdir <- file.path(chm@outDir,chm@name);
    if (is.list(chm@properties)) writePropertiesPost (outdir, chm@format, chm@properties);
    if (chm@format == 'original') {
	shaids <- shaidyGetComponents (chm);
	chmRepo <- file.path (outdir, "shaidyRepo");
	ngchmInitShaidyRepository (chmRepo);
	repo <- shaidyLoadRepository ('file', chmRepo);
	lapply (shaids, function(shaid) {
	    src <- ngchmFindRepo (shaid);
	    shaidyCopyBlob (src, shaid, repo);
	});
	systemCheck (sprintf ("tar cf %s.tar -C %s .", chmRepo, chmRepo));
	unlink (chmRepo, recursive=TRUE);
    }
}

startcust <- paste ("(function(chm){",
	       "function _chm_ad(id,tit,fn){var td=fn($('<div></div>').attr('title',tit).attr('id',id));",
	       " $('body').append(td); $('#'+id).dialog({position:[0,200],autoOpen:false});",
	       " chm.menubar.addDialogsMenuItem(id,tit,function(tlmc,mi){td.dialog();});",
	       "}",
	       "function _chm_as(src){var s=document.createElement('script');",
	       " s.setAttribute('type','text/javascript'); s.setAttribute('src',src);",
	       " $('head').append(s);",
	       "}",
               "function _chm_e(sr,ax,fn){function c2(a,b){return a.concat(b);};",
               " return sr.map(function(r){var v=[];for(var ii=r.start;ii<=r.end;ii++)v.push(ii);",
	       " return v.map(function(i){return fn(ax,i);}).reduce(c2);}).reduce(c2);",
	       "}",
	       "", sep="\n");

# Returns list of all functions in requires and jsfuns.  Required functions come
# before the function(s) needing them.
requiredFunctions <- function (requires, jsfuns) {
    if (length(jsfuns) > 0) {
        for (ff in 1:length(jsfuns)) {
	    fn <- jsfuns[[ff]];
	    if (all(vapply (requires, function(rqfn)rqfn@name!=fn@name, TRUE))) {
		# This fn is not already included
		# First include any of this functions requires.
		rqs <- lapply (fn@requires, function(rq) chmGetFunction(rq));
		requires <- append (requiredFunctions (requires, rqs), fn);
	    }
	}
    }
    requires
}

writeDialogs <- function (dialogs, chan) {
    for (dialog in dialogs) {
        cat (sprintf ("    _chm_ad('%s', '%s', %s);\n", dialog@id, dialog@title, dialog@fn@name), file=chan);
    }
}

writeCustomJS <- function (chm, filename) {
    rqJSfuns <- requiredFunctions (list(), chm@javascript);
    chan <- file (filename, "w");
    if (length(rqJSfuns) > 0) writeJS (rqJSfuns, chan, TRUE);
    cat (startcust, file=chan);
    if (length(rqJSfuns) > 0) writeJS (rqJSfuns, chan, FALSE);
    #cat ("chm.addCustomization(function(){\n", file=chan);
    writeMenu (chm@rowMenu, "row.labels", chan);
    writeMenu (chm@rowMenu, "row.dendrogram", chan);
    writeMenu (chm@colMenu, "column.labels", chan);
    writeMenu (chm@colMenu, "column.dendrogram", chan);
    writeMenu (chm@elementMenu, "matrix", chan);
    writeDialogs (chm@dialogs, chan);
    #cat ("});\n", file=chan);
    cat ("})(MDACC_GLOBAL_NAMESPACE.namespace('tcga').chm);\n", file=chan);
    close (chan);
}

writeJS <- function (js, chan, writeGlobals) {
    for (ii in 1:length(js)) {
	if (js[[ii]]@global == writeGlobals) {
	    cat (sprintf ("%s\n", js[[ii]]@script), file=chan);
	}
    }
}

sameColormap <- function (cmap1, cmap2) {
    if ((class(cmap1) != "ngchmColormap") || (class(cmap2) != "ngchmColormap"))
        stop ("Internal error detected: cmap1 or cmap2 is not a colormap.  Please report.")
    if (cmap1@type != cmap2@type)
        return (FALSE);
    if (length(cmap1@missing) != length(cmap2@missing))
        return (FALSE);
    if ((length(cmap1@missing) > 0) && (cmap1@missing != cmap2@missing))
        return (FALSE);
    if (length(cmap1@points) != length(cmap2@points))
        return (FALSE);
    if (length(cmap1@points) > 0) {
        for (ii in 1:length(cmap1@points)) {
	    if ((cmap1@points[[ii]]@value != cmap2@points[[ii]]@value) ||
	        (cmap1@points[[ii]]@color != cmap2@points[[ii]]@color))
	    {
	        return (FALSE);
	    }
	}
    }
    return (TRUE);
}

# create list representation of layer for output by toJSON
#
prepDataLayer <- function(chm, layer) {
    cmid <- which(vapply (chm@colormaps, function(cmap)sameColormap(cmap,layer@colors), TRUE));
    if (length(cmid) == 0)
        stop (sprintf ("Internal error detected: no color map found for data layer %s. Please report.", layer@name));
    l <- list(name=layer@name, renderer=cmid[[1]]-1, data=layer@data);
    singleElements <- c("name", "renderer");
    for (elem in singleElements) {
            class(l[[elem]]) <- 'singleElement';
    }
    l
}

writeDataLayer <- function (chm, layer, dir, index, chan) {
    prefix = sprintf ("data%d", index);
    cat (sprintf ("%s.file.name=%s.data.tsv\n", prefix, prefix), file=chan);
    cat (sprintf ("%s.label.name=%s\n", prefix, layer@name), file=chan);
    cmid = 0
    if (length(chm@colormaps) > 0) {
        for (ii in 1:length(chm@colormaps)) {
	    if (sameColormap (chm@colormaps[[ii]], layer@colors)) {
	        cmid = ii;
		break;
	    }
	}
    }
    if (cmid == 0)
        stop (sprintf ("Internal error detected: no color map found for data layer %d (%s). Please report.", index, layer@name));
    cat (sprintf ("%s.defaultCM=cm%d\n", prefix, cmid), file=chan);
    repo <- ngchmFindRepo (layer@data);
    layerData <- ngchmLoadDatasetBlob (repo, layer@data)$mat;
    write.table (layerData, file=paste (dir, sprintf("%s.data.tsv", prefix), sep="/"),
                 sep="\t", quote=FALSE);
}

writeCovariateBar <- function (cbar, inpDir, type, index, chan) {
    cat (sprintf ("classification.type%d=%s\n", index, cbar@type), file=chan);
    cat (sprintf ("classification.label%d=%s\n", index, cbar@label), file=chan);
    cat (sprintf ("classification.display%d=%s\n", index, cbar@display), file=chan);
    cat (sprintf ("classification.thickness%d=%d\n", index, cbar@thickness), file=chan);
    if (length (cbar@merge) > 0)
	cat (sprintf ("classification.mergingAlgorithm%d=%s\n", index, cbar@merge), file=chan);
    if (length (cbar@colors) > 0) {
	if (length (cbar@colors@missing) == 0)
	    cbar@colors@missing <- "white";
	writeColorMap ("class", cbar@colors, "classification", sprintf ("%d", index), chan);
    }

    chan2 <- file (paste (inpDir, sprintf ("%sClassificationData%d.txt", type, index), sep="/"), "w")
    repo <- ngchmFindRepo (cbar@data);
    barData <- ngchmLoadDatasetBlob (repo, cbar@data, "")$mat;
    nm <- rownames(barData)
    for (ii in 1:nrow(barData))
        cat (nm[ii], "\t", barData[ii,1], "\n", sep="", file=chan2);
    close (chan2);
}

addDefaultCovariate <- function (covariates, labels)
{
    if (!("None" %in% vapply (covariates, function(cov)cov@label, ""))) {
	series <- rep ("default", length(labels));
	names(series) <- labels;
	cmap <- chmNewColorMap ("default", colors="black", names="Point");
	cov <- chmNewCovariate ("Nothing", series, value.properties=cmap, type='discrete', covabbv='None');
        covariates <- append (covariates, cov);
    }
    covariates
}

#' @import tsvio
writeDataset <- function (chm, dataset, dir) {
    chm@extrafiles <- c(chm@extrafiles, sprintf ("%s.tsv", dataset@name));
    chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-index.tsv", dataset@name));

    write.table (dataset@data, file.path (dir, sprintf ("%s.tsv", dataset@name)), sep="\t", quote=FALSE);
    tsvio::tsvGenIndex (file.path (dir, sprintf ("%s.tsv", dataset@name)),
                        file.path (dir, sprintf ("%s-index.tsv", dataset@name)));

    row.covars <- addDefaultCovariate (dataset@row.covariates, rownames(dataset@data));
    col.covars <- addDefaultCovariate (dataset@column.covariates, colnames(dataset@data));
    if (TRUE) {
	chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-covariates.tsv", dataset@name));
	cov.table <- list(Covariate=vapply(col.covars, function(cov)cov@label, ""),
	                  Fullname=vapply(col.covars, function(cov)cov@fullname, ""));
	write.table(cov.table,
		    file.path (dir, sprintf ("%s-covariates.tsv", dataset@name)),
		    sep="\t", quote=FALSE, row.names=FALSE);
    }
    if (TRUE) {
	chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-rowcovariates.tsv", dataset@name));
	cov.table <- list(Covariate=vapply(row.covars, function(cov)cov@label, ""),
	                  Fullname=vapply(row.covars, function(cov)cov@fullname, ""));
	write.table(cov.table,
		    file.path (dir, sprintf ("%s-row-covariates.tsv", dataset@name)),
		    sep="\t", quote=FALSE, row.names=FALSE);
    }


    if (TRUE) {
	first.rowser <- TRUE;
	first.serprop <- TRUE;
	for (cov in row.covars) {
        repo <- ngchmFindRepo (cov@label.series);
        label.series <- ngchmLoadDatasetBlob (repo, cov@label.series)$mat[,'Value'];
	    rowser <- list (Sample=names(label.series), Series=label.series, Covariate=rep(cov@label,length(label.series)));
	    if (first.rowser) {
	        first.rowser <- FALSE;
		chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-row-series.tsv", dataset@name));
		fd.rowser <- file (file.path (dir, sprintf ("%s-row-series.tsv", dataset@name)), "w");
		write.table(rowser, file=fd.rowser, sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE);
	    } else {
		write.table(rowser, file=fd.rowser, sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE);
	    }
	    if (length(cov@series.properties) > 0) {
		serprop <- getSeriesProps (cov@label, cov@series.properties);
		if (first.serprop) {
		    first.serprop <- FALSE;
		    chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-row-series-properties.tsv", dataset@name));
		    fd.serprop <- file (file.path (dir, sprintf ("%s-row-series-properties.tsv", dataset@name)), "w");
		    write.table(serprop, file=fd.serprop, sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE);
		} else {
		    write.table(serprop, file=fd.serprop, sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE);
		}
	    }
	}
	if (!first.rowser) close (fd.rowser);
	if (!first.serprop) close (fd.serprop);
    }

    if (TRUE > 0) {
	first.colser <- TRUE;
	first.serprop <- TRUE;
	for (cov in col.covars) {
        repo <- ngchmFindRepo (cov@label.series);
        label.series <- ngchmLoadDatasetBlob (repo, cov@label.series)$mat[,'Value'];
	    colser <- list (Sample=names(label.series), Series=label.series, Covariate=rep(cov@label,length(label.series)));
	    if (first.colser) {
	        first.colser <- FALSE;
		chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-sample-series.tsv", dataset@name));
		fd.colser <- file (file.path (dir, sprintf ("%s-sample-series.tsv", dataset@name)), "w");
		write.table(colser, file=fd.colser, sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE);
	    } else {
		write.table(colser, file=fd.colser, sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE);
	    }
	    if (length(cov@series.properties) > 0) {
		serprop <- getSeriesProps (cov@label, cov@series.properties);
		if (first.serprop) {
		    first.serprop <- FALSE;
		    chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-series-properties.tsv", dataset@name));
		    fd.serprop <- file (file.path (dir, sprintf ("%s-series-properties.tsv", dataset@name)), "w");
		    write.table(serprop, file=fd.serprop, sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE);
		} else {
		    write.table(serprop, file=fd.serprop, sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE);
		}
	    }
	}
	if (!first.colser) close (fd.colser);
	if (!first.serprop) close (fd.serprop);
    }

    chm
}

hasSeries <- function (props, value)
{
    value %in% vapply (props, function(p) as.character(p@value), "")
}

addDefaultCovariateProperties <- function (props, missing.color, default.missing.color)
{
    if (!hasSeries (props, "unspecified")) {
	if (length (missing.color) == 0) missing.color <- default.missing.color;
        props <- chmAddValueProperty (props, value="unspecified", name="Unspecified", color=missing.color, shape="triangle-down", z=1);
    }
    if (!hasSeries (props, "regression")) {
        props <- chmAddValueProperty (props, value="regression", name="Regression", color="red", shape="line", z=1000);
    }
    props
}

getSeriesProps <- function (label, props)
{
    if (class (props) == "ngchmColormap") {
        pts <- addDefaultCovariateProperties (props@points, props@missing, "black");
	list (Covariate=vapply(pts,function(pt)label,""),
	      Series=vapply(pts,function(pt)as.character(pt@value),""),
	      Description=vapply(pts,function(pt)pt@name,""),
	      Color=vapply(pts,function(pt)pt@color,""),
	      Shape=vapply(pts,function(pt)pt@shape,""),
	      zIndex=vapply(pts,function(pt)pt@z,1))
    } else {
	append (list(Covariate=rep(label, length(props[[1]]))), props)
    }
}

writeTemplate <- function (source.path, dest.path, substitutions, outDir) {
    if ((class(source.path)=="character") && (length(substitutions) == 0)) {
	if (!file.copy (source.path, file.path (outDir, dest.path))) {
	    stop (sprintf ("Unable to copy template file '%s' to '%s'", source.path,
	                   file.path (outDir, dest.path)));
	}
        #systemCheck (sprintf ("/bin/cp %s %s",
	#                       shQuote (source.path),
	#		       shQuote (file.path (outDir, dest.path))));
    } else {
	if (class(source.path)=="character") {
	    data <- readLines (source.path);
	} else {
	    data <- source.path ();
	}
	for (ss in substitutions)
	    data <- gsub (ss[1], ss[2], data);
	writeLines (data, con=file.path (outDir, dest.path));
    }
}

writeRelatedGroup  <- function (group, links, chan) {
    cat (sprintf ("  { header: '%s',\n", group@header), file=chan);
    if (length(group@blurb) > 0)
	cat (sprintf ("    blurb: '%s',\n", group@blurb), file=chan);
    cat (sprintf ("    %s: [\n", group@linktype), file=chan);
    for (ii in 1:length(links)) {
	if (links[[ii]]@group == group@name)
            cat ('      { link: "', links[[ii]]@link, '", description: "', links[[ii]]@description, '" },\n', sep='', file=chan);
    }
    cat ("    ]\n", file=chan);
    cat ("  },\n", file=chan);
}

writeRelated  <- function (groups, links, outdir) {
    chan <- file (file.path (outdir, "relatedlinks.js"), "w");
    cat ("linkoutData = { groups: [\n", file=chan);
    for (ii in 1:length(groups))
        writeRelatedGroup (groups[[ii]], links, chan);
    cat ("]};\n", file=chan);
    close (chan);
    NULL
}

# Write extra support files to the specified directory
writeChmExtraSupport <- function (chm, chmSaveDir)
{
    if ((length(chm@relatedLinks)+length(chm@relatedGroups)) > 0) {
	writeRelated (chm@relatedGroups, chm@relatedLinks, chmSaveDir);
    }
    if (chm@format=='original' && length(chm@datasets) > 0) {
	chan <- file (file.path (chmSaveDir, "datasets.tsv"), "w");
	writeLines ("Dataset\tDescription", con=chan);
	for (ii in 1:length(chm@datasets)) {
	    ds <- chm@datasets[[ii]];
	    chm <- writeDataset (chm, ds, chmSaveDir);
	    writeLines (sprintf ("%s\t%s", ds@name, ds@description), con=chan);
	}
	close (chan);
    }
    if (chm@format=='original' && length(chm@templates) > 0) {
	for (t in chm@templates)
	    writeTemplate (t@source.path, t@dest.path, t@substitutions, chmSaveDir);
    }
    chm
}

getTypeMatches <- function (tflist, type) {
    # tflist$types is a list of character vectors.
    # type is a list of character vectors.
    # Returns the indices of the elements of tflist whose types match at least one type.
    idx <- which(vapply (tflist$types, function(tt)any(tt==type), TRUE));
}

getValueExpr <- function (tflist, type, where) {
    idx <- getTypeMatches (tflist, type);
    if (length (idx) == 0)
        stop (sprintf ("chmMake: internal error detected: unable to find value expression for type '%s'. Please report.", type));
    b <- tflist$builders[[idx[1]]];
    if (class(b) == "ngchmAxisType") {
	if (where == "axis") {
	    return (sprintf ("_chm_e(s,a,%s)", b@func@name));
	}
	else if (where == "row") {
	    return (sprintf ("_chm_e(rs,chm.row,%s)", b@func@name));
	}
	else if (where == "column") {
	    return (sprintf ("_chm_e(cs,chm.column,%s)", b@func@name));
	}
	else {
	    stop (sprintf ("chmMake: internal error detected: unknown getValueExpr location '%s'. Please report.", where));
	}
    } else if (class(b) == "ngchmTypeMapper") {
        if (b@op == "expr") {
            return (sprintf ("%s.%s", getValueExpr(tflist, b@fromtype, where), b@params$expr));
        } else if (b@op == "field") {
            return (sprintf ("%s.split(%s)[%s]", getValueExpr(tflist, b@fromtype, where), b@params$separator, b@params$num));
        } else if (b@op == "javascript") {
            return (sprintf ("%s(%s)", b@func@name, getValueExpr(tflist, b@fromtype, where)));
        } else {
            stop ("unknown ngchmTypeMapper op ", b@op);
        }
    } else {
        stop (sprintf ("chmMake: internal error detected: unknown value builder class '%s'. Please report.", class(b)));
    }
}

getFnsRqrd <- function (tflist, type) {
    idx <- getTypeMatches (tflist, type);
    if (length (idx) == 0)
        stop (sprintf ("chmMake: internal error detected: unable to find value expression for type '%s'. Please report.", type));
    b <- tflist$builders[[idx[1]]];
    if (class(b) == "ngchmTypeMapper") {
        return (c (idx, getFnsRqrd(tflist, b@fromtype)));
    } else {
        return (idx);
    }
}

writeChm <- function (chm, saveDir=NULL) {
    if (length (chm@layers) == 0)
        stop ("The NGCHM has no data layers. You must add at least one.");
    if (length (chm@colormaps) == 0)
        stop ("Internal error detected: the NGCHM has no color maps.  Please report.");

    #chm <- chmAddAutoMenuItems (chm);
    genSpecFeedback (50, "creating specification directory");
    if (length(saveDir)==0) {
        unlink (chm@inpDir, recursive=TRUE);
        if (!dir.create (chm@inpDir, recursive=TRUE)) {
            stop (sprintf ("Unable to create directory '%s' in which to save CHM specification", chm@inpDir));
        }
        #system (sprintf ("/bin/rm -rf %s", chm@inpDir));
        #systemCheck (sprintf ("/bin/mkdir %s", chm@inpDir));
        saveDir <- chm@inpDir;
    }

    if (chm@format == "original") {
        genSpecFeedback (55, "saving user's CHM");
        orig.chm <- chm;
        chm@inpDir <- chm@outDir <- chm@saveDir <- "";
        save (chm, file=file.path (saveDir, "chm.Rdata"));
        chm <- orig.chm;
        chm@extrafiles <- c (chm@extrafiles, "chm.Rdata");
    }

    if (chm@format == "original") {
        genSpecFeedback (60, "writing specification");
	props <- file (file.path (saveDir, chm@propFile), "w");
	cat (sprintf ("# This NGCHM property description was produced using the R NGCHM library version %s at %s\n",
		      packageDescription("NGCHM")$Version, date()), file=props);
	cat (sprintf ("data.set.name=%s\n", chm@name), file=props);
	cat (sprintf ("chm.main.image.height=%d\n", chm@height), file=props);
	cat (sprintf ("chm.main.image.width=%d\n", chm@width), file=props);
    } else {
        props <- list(name=chm@name);
    }

    if (length (chm@tags) > 0) {
	if (chm@format=="original") {
            cat (sprintf ("tags=%s\n", paste(chm@tags,sep=",",collapse=",")), file=props);
	} else {
	    props$tags <- chm@tags;
	}
    }

    if (chm@format=="original") {
	genSpecFeedback (65, "writing color schemes");
	for (ii in 1:length(chm@colormaps)) {
	    cmap <- chm@colormaps[[ii]];
	    if (length (cmap@missing) == 0)
		cmap@missing <- "white";
	    writeColorMap ("main", cmap, sprintf("colormap%d", ii), "", props);
	}
    } else {
        props$colormaps <- lapply (chm@colormaps, function(cmap) {
	    if (length (cmap@missing) == 0)
		cmap@missing <- "white";
	    jsonColorMap ("main", cmap)
	});
	names(props$colormaps) <- sprintf ("colormap%d", 1:length(chm@colormaps));
    }

    if (chm@format=="original") {
        genSpecFeedback (70, "writing data layers");
        for (ii in 1:length(chm@layers))
            writeDataLayer (chm, chm@layers[[ii]], saveDir, ii, props);
    }

    if (is.list(chm@properties)) {
	if (chm@format == "original") {
	    writeProperties (saveDir, chm@format, chm@properties, props);
	}
	if (chm@format == "original" && hasSpecialProperties (chm)) {
	    fname <- if (chm@format=="original") "extra.properties" else "extra-properties.json";
	    chm@extrafiles <- c (chm@extrafiles, fname);
	    extraprops <- file (file.path (saveDir, fname), "w");
	    writeProperties (saveDir, chm@format, chm@properties, extraprops, TRUE);
	    close (extraprops);
	}
    }

    if (chm@format == "original") {
	if (is.list(chm@overviews)) {
	    for (ii in 1:length(chm@overviews)) {
		ov <- chm@overviews[[ii]];
		cat (sprintf ("overview%d.format=%s\n", ii, ov@format), file=props);
		if (!is.null(ov@width))
		    cat (sprintf ("overview%d.width=%d\n", ii, ov@width), file=props);
		if (!is.null(ov@height))
		    cat (sprintf ("overview%d.height=%d\n", ii, ov@height), file=props);
	    }
	}
    }
    genSpecFeedback (80, "writing extra support files");
    chm <- writeChmExtraSupport (chm, saveDir);
    chm@extrafiles <- c(chm@extrafiles, "custom-backup.js");

    if (chm@format == "original") {
        if (length (chm@extrafiles) > 0)
            cat (sprintf ("additional.input=%s\n", paste(chm@extrafiles,sep="",collapse=",")), file=props);
        close (props);
    }

    if (chm@format == "original") {
	genSpecFeedback (90, "writing covariate bar data");
	if (!is.null(chm@rowOrder))
	    writeOrder (saveDir, "row", chm@rowOrder);
	if (!is.null(chm@colOrder))
	    writeOrder (saveDir, "column", chm@colOrder);
	if (!is.null(chm@rowMeta))
	    writeMeta (saveDir, "row", chm@rowMeta);
	if (!is.null(chm@colMeta))
	    writeMeta (saveDir, "column", chm@colMeta);
	if (is.list (chm@rowCovariateBars)) {
	    chan <- file (paste (saveDir, "rowClassification1.txt", sep="/"), "w");
	    for (ii in 1:length(chm@rowCovariateBars) )
		writeCovariateBar (chm@rowCovariateBars[[ii]], saveDir, "row", ii, chan);
	    close (chan);
	}
	if (is.list(chm@colCovariateBars)) {
	    chan <- file (paste (saveDir, "columnClassification1.txt", sep="/"), "w");
	    for (ii in 1:length(chm@colCovariateBars))
		writeCovariateBar (chm@colCovariateBars[[ii]], saveDir, "column", ii, chan);
	    close (chan);
	}
    }

    if (chm@format == "original") {
        genSpecFeedback (95, "writing custom CSS and Javascript");
        if (is.list(chm@css)) writeCSS (chm@css, saveDir);
        chmWriteCustomJS (chm, file.path (saveDir, "custom-backup.js"));
        jsloader <- readLines(system.file("extdata", "custom.js", package="NGCHM"));
        jsfile <- file (file.path (saveDir, "custom.js"), "w");
        writeLines (jsloader, jsfile);
        close (jsfile);
    }

    if (chm@format=="shaidy") {
        writeLines (jsonlite::toJSON(chm), file.path(saveDir, "chm.json"));
    }
}

#' @rdname chmName-method
#' @aliases chmName,ngchm-method
#'
setMethod ("chmName",
           signature = c(chm="ngchm"),
	   definition = function (chm) chm@name);

writeOrder <- function (inpDir, type, ord) {
    # Write the order/dendrogram out as a column dendrogram to the inpDir
    if (is(ord, "shaid")) {
        repo <- ngchmFindRepo (ord);
        if (ord@type == 'dendrogram') {
	    blobfile <- repo$blob.path (ord, 'dendrogram-data.tsv');
	    filename <- file.path (inpDir, sprintf ("dendrogram-data_%s.tsv", type));
	    stopifnot (file.copy (blobfile, filename));
	    blobfile <- repo$blob.path (ord, 'dendrogram-order.tsv');
	    filename <- file.path (inpDir, sprintf ("dendrogram-order_%s.tsv", type));
	    stopifnot (file.copy (blobfile, filename));
	    # For legacy
	    blobfile <- repo$blob.path (ord, 'dendrogram.str');
	    filename <- file.path (inpDir, sprintf ("dendro_%s.str", type));
	    stopifnot (file.copy (blobfile, filename));
        } else if (ord@type == 'label') {
	    blobfile <- repo$blob.path (ord, 'labels.txt');
	    filename <- file.path (inpDir, sprintf ("%s.txt", type));
	    stopifnot (file.copy (blobfile, filename));
        } else {
            stop ('Unexpected shaid type: ', ord@type);
        }
    } else if (class (ord) == "character") {
	filename <- file.path (inpDir, sprintf ("%s.txt", type));
        write.table (ord, filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
    } else if ((class (ord) == "dendrogram") || (class (ord) == "hclust")) {
	sink (file.path (inpDir, sprintf ("dendro_%s.str", type)))
	if (class (ord) == "hclust")
	    ord <- as.dendrogram (ord);
	str (ord)
	sink (NULL)
    } else if (class (ord) == "fileContent") {
	filename <- (paste (inpDir, sprintf ("dendro_%s.str", type), sep="/"));
	ff <- file (filename, "w");
	writeLines (ord, ff);
	close (ff);
    } else if (class (ord) == "file") {
	stop ("Internal error detected: axis order type file should not be here. Please report.");
	filename <- (paste (inpDir, sprintf ("dendro_%s.str", type), sep="/"));
	content <- readLines (ord);
	ff <- file (filename, "w");
	writeLines (content, ff);
	close (ff);
    } else if (class (ord) == "NULL") {
        # Do nothing.
    } else {
        stop (sprintf ("chmWriteOrder: unknown class of %s order: '%s'", type, class(ord)));
    }
}

writeMeta <- function (inpDir, type, metadata) {
    # Write the metadata out to the inpDir
    data <- lapply (metadata, function (shaid) {
        stopifnot (is (shaid, "shaid"));
        repo <- ngchmFindRepo (shaid);
        meta <- ngchmLoadDatasetBlob (repo, shaid)$mat;
        meta[,'Value']
    });
    labels <- sort(unique(do.call(c,lapply (data, function(x)names(x)))));
    proto <- rep (NA, length(labels));
    names(proto) <- labels;
    data <- do.call (rbind, lapply (data, function (cv) {
        p <- proto;
        p[names(cv)] <- cv;
        p
    }));
    filename = sprintf ("%s/%s_meta.txt", inpDir, type);
    write.table (data, filename, quote=FALSE, row.names=FALSE, col.names=TRUE, sep="\t");
}

prepChmOrderings <- function (chm, l) {
    # Fix row order
    if (length(chm@rowOrder)==0) {
        l$rowOrder <- ngchmGetLabels(chm@layers[[1]]@data,"row");
    } else if (!is(chm@rowOrder,"shaid")) {
        stop (sprintf ("For chm %s unknown class for row order: %s", chm@name, class(chm@rowOrder)));
    } else if (chm@rowOrder@type == 'label') {
        # Nothing to do.
    } else if (chm@rowOrder@type == 'dendrogram') {
        l$rowDendrogram <- l$rowOrder;
        l$rowOrder <- ngchmGetLabels(chm@rowOrder)[[1]];
    } else {
        stop (sprintf ("For chm %s unknown shaid type for row order: %s", chm@name, chm@rowOrder@type));
    }
    # Repeat for col order
    if (length(chm@colOrder)==0) {
        l$colOrder <- ngchmGetLabels(chm@layers[[1]]@data,"column");
    } else if (!is(chm@colOrder,"shaid")) {
        stop (sprintf ("For chm %s unknown class for column order: %s", chm@name, class(chm@colOrder)));
    } else if (chm@colOrder@type == 'label') {
        # Nothing to do.
    } else if (chm@colOrder@type == 'dendrogram') {
        l$colDendrogram <- l$colOrder;
        l$colOrder <- ngchmGetLabels(chm@colOrder)[[1]];
    } else {
        stop (sprintf ("For chm %s unknown shaid type for column order: %s", chm@name, chm@colOrder@type));
    }
    l
}

#' @rdname chmGetURL-method
#' @param server The server on which to view the NGCHM
#' @param ... Ignored.
#' @aliases chmGetURL,character-method
setMethod ("chmGetURL",
    signature = c(chm="character"),
    definition = function (chm, server=NULL, ...) {
        if (length(server)==0) server <- chmCurrentServer();
        stopifnot(length(server) > 0);
        if (typeof(server) == 'character') server <- chmServerCheck (server);
        sprintf ("%s/chm.html?name=%s",
                 if (length(server@viewServer)>0) server@viewServer else server@serverURL,
                 chm)
});

#' @rdname chmGetURL-method
#' @param server The server on which to view the NGCHM
#' @aliases chmGetURL,ngchm-method
setMethod ("chmGetURL",
    signature = c(chm="ngchm"),
    definition = function (chm, server=NULL, ...) {
        if (length(server)==0) server <- chmCurrentServer();
        stopifnot(length(server) > 0);
        if (typeof(server) == 'character') server <- chmServerCheck (server);
        if (server@serverProtocol@chmFormat == 'shaidy') {
            sprintf ("%s/chm.html?map=%s",
                     if (length(server@viewServer)>0) server@viewServer else server@serverURL,
                     shaidyGetShaid (chm)@value)
        } else {
            chmGetURL (chmName (chm), server=server, ...)
        }
});

URLparts <- function(x) {
    m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
    parts <- do.call(rbind,
		     lapply(regmatches(x, m), `[`, c(3L, 4L, 6L, 7L)))
    colnames(parts) <- c("protocol","host","port","path")
    parts
}

datestamp <- function () {
    format(Sys.time(), "%a %b %d %X %Y")
}

progressFeedback <- function(progress, mode, what)
{
    cat (sprintf ("%s\t%s\t:%s:\t%g\t%s\n", datestamp(), "PROGRESS", mode, progress, what), file=stderr());
}

genSpecFeedback <- function (progress, what)
{
    progressFeedback (progress, "Writing specification", what);
}

postBuildFeedback <- function (progress, what)
{
    progressFeedback (progress, "Post build", what);
}

addToolBoxes <- function (chm)
{
    type.matches <- function (dstype, chmtype) {
	length (intersect (dstype, chmtype)) > 0
    }
    type.matches2 <- function (dstype, chmtype1, chmtype2) {
	length (intersect (intersect (dstype, chmtype1), chmtype2)) > 0
    }
    t2s <- function (ty) {
        paste (ty, collapse='/')
    }

    rowtypes <- getAllAxisTypes (chm, "row");
    matches <- vapply (chm@datasets, function(ds) type.matches (ds@row.type, rowtypes$types), TRUE);
    cat (sprintf ("addToolBoxes: found %d R datasets matching row types:\n", sum(matches)), file=stderr());
    if (sum(matches) > 0) {
	if (sum(matches) == 1) {
	    extra <- "";
	} else {
	    extra <- sprintf (" (%s)", vapply(chm@datasets[matches], function(ds)ds@name, ""));
	}
	for (ds in chm@datasets[matches]) {
	    cat (sprintf ("dataset '%s' row.type '%s'\n", ds@name, t2s(ds@row.type)), file=stderr());
	    chm <- chmAddToolboxR (chm, "row", ds@row.type, ds@name, extra[1]);
	    extra <- tail (extra, -1);
	}
    }
    coltypes <- getAllAxisTypes (chm, "column");
    matches <- vapply (chm@datasets, function(ds) type.matches (ds@row.type, coltypes$types), TRUE);
    cat (sprintf ("addToolBoxes: found %d R datasets matching column types:\n", sum(matches)), file=stderr());
    if (sum(matches) > 0) {
	if (sum(matches) == 1) {
	    extra <- "";
	} else {
	    extra <- sprintf (" (%s)", vapply(chm@datasets[matches], function(ds)ds@name, ""));
	}
	for (ds in chm@datasets[matches]) {
	    cat (sprintf ("dataset '%s' row.type '%s'\n", ds@name, t2s(ds@row.type)), file=stderr());
	    chm <- chmAddToolboxR (chm, "column", ds@row.type, ds@name, extra[1]);
	    extra <- tail (extra, -1);
	}
    }
    matches <- vapply (chm@datasets, function(ds) type.matches2 (ds@row.type, coltypes$types, rowtypes$types), TRUE);
    cat (sprintf ("addToolBoxes: found %d R2 datasets matching row and column types:\n", sum(matches)), file=stderr());
    if (sum(matches) > 0) {
	if (sum(matches) == 1) {
	    extra <- "";
	} else {
	    extra <- sprintf (" (%s)", vapply(chm@datasets[matches], function(ds)ds@name, ""));
	}
	for (ds in chm@datasets[matches]) {
	    cat (sprintf ("dataset '%s' row.type '%s'\n", ds@name, t2s(ds@row.type)), file=stderr());
	    chm <- chmAddToolboxR2 (chm, ds@row.type, ds@name, extra[1]);
	    extra <- tail (extra, -1);
	}
    }
    matches <- vapply (chm@datasets, function(ds) type.matches(ds@column.type, coltypes$types) && type.matches(ds@row.type, rowtypes$types), TRUE);
    cat (sprintf ("addToolBoxes: found %d RC datasets matching row and column types:\n", sum(matches)), file=stderr());
    if (sum(matches) > 0) {
	if (sum(matches) == 1) {
	    extra <- "";
	} else {
	    extra <- sprintf (" (%s)", vapply(chm@datasets[matches], function(ds)ds@name, ""));
	}
	for (ds in chm@datasets[matches]) {
	    cat (sprintf ("dataset '%s' row.type '%s' col.type '%s'\n", ds@name, t2s(ds@row.type), t2s(ds@column.type)), file=stderr());
	    chm <- chmAddToolboxRC (chm, ds@row.type, ds@column.type, ds@name, extra[1]);
	    extra <- tail (extra, -1);
	}
    }
    chm
}

#' @rdname chmMake-method
#' @aliases chmMake,ngchm-method
#'
setMethod ("chmMake",
    signature = c(chm="ngchm"),
    definition = function (chm, ...) {

    chm <- chmFixVersion (chm);
    # Compute row and column orders if required.
    while ((length(chm@rowOrder) > 0) && (class(chm@rowOrder) == "function")) {
	genSpecFeedback (0, "determining row order");
        chm@rowOrder <- chm@rowOrder (chm);
    }
    if (length(chm@rowOrder)==0) {
        chm@rowOrder <- chmOriginalRowOrder (chm);
    } else if (is(chm@rowOrder,"dendrogram")) {
        chm@rowOrder <- chmUserDendrogramToShaid (chm@rowOrder);
    } else if (is(chm@rowOrder,"character")) {
        chm@rowOrder <- chmUserLabelsToShaid (chm@rowOrder);
    }
    while ((length(chm@colOrder) > 0) && (class(chm@colOrder) == "function")) {
	genSpecFeedback (10, "determining column order");
        chm@colOrder <- chm@colOrder (chm);
    }
    if (length(chm@colOrder)==0) {
        chm@colOrder <- chmOriginalColOrder (chm);
    } else if (is(chm@colOrder,"dendrogram")) {
        chm@colOrder <- chmUserDendrogramToShaid (chm@colOrder);
    } else if (is(chm@colOrder,"character")) {
        chm@colOrder <- chmUserLabelsToShaid (chm@colOrder);
    }
    chm
});


#' Make an original format NGCHM.
#'
#' @param chm The original format CHM to compile.
#' @param server The server for which to compile the NGCHM.
#'        Default getOption("NGCHM.Server",chmListServers()[1]).
#'        Required iff useJar is not defined.
#' @param deleteOld If TRUE, delete any old CHM of this name before beginning build. (Default is TRUE.)
#' @param useJAR If defined, the location (filename) of the chmbuilder jar file. The package will not download
#'        a current jar file from the server. It is the caller's responsibility to ensure the builder jar file
#'        is compatible with the server on which the NGCHM will be installed. (Default is not defined.)
#' @param javaOptions Additional options to pass to the Java process.
#'        Default is getOption('NGCHM.Java.Options','-Xmx2G').
#' @param javaTraceLevel Trace level option passed to the Java process.
#'        Default is getOption("NGCHM.Java.Trace','PROGRESS').
#' @param buildArchive If TRUE, build a tar archive of the generated NGCHM.
#'        Default is getOption('NGCHM.Build.Archive',TRUE).
#'
#' @return The CHM
ngchmMakeFormat.original <- function (chm,
                                      server,
                                      deleteOld=TRUE,
                                      useJAR=NULL,
                                      javaTraceLevel=NULL,
                                      javaOptions=NULL,
                                      buildArchive=NULL
) {

    if (length(javaTraceLevel)==0) javaTraceLevel <- getOption("NGCHM.Java.Trace", "PROGRESS");
    if (length(javaOptions)==0) javaOptions <- getOption("NGCHM.Java.Options", "-Xmx2G");
    if (length(server)==0) server <- chmCurrentServer();
    if (length(buildArchive)==0) buildArchive <- getOption("NGCHM.Build.Archive", TRUE);

    genSpecFeedback (20, "writing NGCHM specification");
    writeChm (chm);

    genSpecFeedback (96, "preparing output directory");
    dir.create (chm@outDir, recursive=TRUE, showWarnings=FALSE);
    if (deleteOld) {
        unlink (file.path (chm@outDir, chm@name), recursive=TRUE);
    }

    if (length(useJAR) == 0) {
        genSpecFeedback (97, "retrieving NGCHM rendering software");
	useJAR <- getBuilderJar (server);
    }
    genSpecFeedback (100, "rendering NGCHM");
    #
    javaTraceOpts <- ""
    if ((length(javaTraceLevel) > 0) && (is.null(server) || (length(server@traceLevel)>0))) {
	javaTraceOpts <- sprintf ("-l %s -p", shQuote(javaTraceLevel));
    }

    systemCheck (sprintf ("java -Djava.awt.headless=true %s -jar %s %s %s %s/%s %s",
		  paste (vapply (javaOptions, shQuote, ""), collapse=" "),
		  shQuote (useJAR),
		  javaTraceOpts,
		  shQuote (chm@inpDir),
		  shQuote (chm@inpDir),
		  shQuote (chm@propFile),
		  shQuote (chm@outDir)));
    cat ("chmMake: Java process completed\n", file=stderr());

    postBuildFeedback (0, "writing post build files");
    writeChmPost (chm);
    if (buildArchive) {
	postBuildFeedback (50, "creating compressed NGCHM file");
	systemCheck (sprintf ("tar czf %s/%s.ngchm.gz -C %s %s",
			     shQuote (chm@saveDir),
			     shQuote (chm@name),
			     shQuote (chm@outDir),
			     shQuote (chm@name)));
    }
    postBuildFeedback (100, "post build completed");
    chm
};

#' @rdname chmAdd-method
#' @aliases chmAdd,ngchm-method
#'
setMethod ("chmAdd",
    signature = c(chm="ngchm"),
    definition = function (chm, ...) {
        chm <- chmFixVersion (chm);
	chmAddList (chm, list (...))
});

chmOperatorAdd <- function (left, right) {
    if (is (left, "ngchm")) {
        chmAdd (left, right)
    }
    else if (is (right, "ngchm")) {
        chmAdd (right, left)
    }
    else if (is (left, "ngchmAxis")) {
        if (is (right, "ngchmAxis")) {
	    stopifnot (left@axis == right@axis);
            left@objects <- append (left@objects, right@objects);
        } else {
            left@objects <- append (left@objects, right);
        }
        left
    }
    else if (is (right, "ngchmAxis")) {
	right@objects <- append (right@objects, left);
        right
    }
    else {
        stop ("unknown object class")
    }
}

setMethod ("+",
           signature=c(e1="ngchmVersion2", e2="ngchmAxis"),
           definition = function(e1,e2) chmOperatorAdd (e1, e2));

#' @method + ngchmVersion2
#' @export
"+.ngchmVersion2" <- chmOperatorAdd

#' @method + ngchmAxis
#' @export
"+.ngchmAxis" <- chmOperatorAdd

#' @method dimnames ngchmVersion2
#' @export
dimnames.ngchmVersion2 <- function(chm) {
    if (length(chm@layers) == 0) {
        NULL
    } else {
        dimnames (chm@layers[[1]])
    }
}

#' @method dim ngchmVersion2
#' @export
dim.ngchmVersion2 <- function(chm) {
    vapply (dimnames(chm), length, 0)
}

#' @method dimnames ngchmLayer
#' @export
dimnames.ngchmLayer <- function(ll) {
    list (ngchmGetLabelsStr (ll@data,"row"), ngchmGetLabelsStr (ll@data,"column"))
}

#' @method dim ngchmLayer
#' @export
dim.ngchmLayer <- function(ll) {
    vapply (dimnames(ll), length, 0)
}

#' @rdname chmAddLayer-method
#' @aliases chmAddLayer,ngchm,ngchmLayer-method
#'
setMethod ("chmAddLayer",
    signature = c(chm="ngchm", layer="ngchmLayer"),
    definition = function (chm, layer) {
        chm <- chmFixVersion (chm);
	validateNewLayer (chm, layer);
	chm@layers <- append (chm@layers, layer);
        chmAddColormap (chm, layer@colors)
});

#' @rdname chmAddLayer-method
#' @aliases chmAddLayer,ngchm,matrix-method
#'
setMethod ("chmAddLayer",
    signature = c(chm="ngchm", layer="matrix"),
    definition = function (chm, layer) {
        chm <- chmFixVersion (chm);
	layer <- chmNewDataLayer (sprintf ("Layer %d", length(chm@layers)+1), layer);
	validateNewLayer (chm, layer);
	chm@layers <- append (chm@layers, layer);
        chmAddColormap (chm, layer@colors)
});

#' @rdname chmAddCSS-method
#' @aliases chmAddCSS,ngchm,character,character-method
setMethod ("chmAddCSS",
    signature = c(chm="ngchm", css="character"),
    definition = function (chm, css) {
        chm <- chmFixVersion (chm);
	chm@css <- append (chm@css, new (Class="ngchmCSS", css=css));
        chmUU (chm)
});

#' @rdname chmAddTag-method
#' @aliases chmAddTag,ngchm,character,character-method
setMethod ("chmAddTag",
    signature = c(chm="ngchm", tag="character"),
    definition = function (chm, tag) {
        chm <- chmFixVersion (chm);
	chm@tags <- c (chm@tags, tag);
        chmUU (chm)
});

#' @rdname chmAddDataset-method
#' @aliases chmAddDataset,ngchm,ngchmDataset-method
setMethod ("chmAddDataset",
    signature = c(chm="ngchm", dataset="ngchmDataset"),
    definition = function (chm, dataset) {
        chm <- chmFixVersion (chm);
	if (length(chm@datasets) == 0) {
	    chm@extrafiles <- c(chm@extrafiles, "datasets.tsv");
	}
	chm@datasets <- append (chm@datasets, dataset);
	if (length(dataset@row.type) > 0)
	    chm <- chmAddProperty (chm, sprintf ("!datasettype:%s-row", make.names(dataset@name)), dataset@row.type);
	if (length(dataset@column.type) > 0)
	    chm <- chmAddProperty (chm, sprintf ("!datasettype:%s-column", make.names(dataset@name)), dataset@column.type);
        chmUU (chm)
});

#' @rdname chmAddDialog-method
#' @aliases chmAddDialog,ngchm,ngchmDialog-method
setMethod ("chmAddDialog",
    signature = c(chm="ngchm", dialog="ngchmDialog"),
    definition = function (chm, dialog) {
        chm <- chmFixVersion (chm);
	if (dialog@id %in% vapply(chm@dialogs, function(d)d@id, "")) {
	    stop (sprintf ("A dialog with id '%s' already exists", dialog@id));
	}
	if (dialog@title %in% vapply(chm@dialogs, function(d)d@title, "")) {
	    stop (sprintf ("A dialog with title '%s' already exists", dialog@title));
	}
	chm@dialogs <- append (chm@dialogs, dialog);
	chmUU (addFunDefine (chm, dialog@fn))
});

#' @rdname chmAddCovariate-method
#' @aliases chmAddCovariate,ngchmDataset,character,ngchmCovariate-method
setMethod ("chmAddCovariate",
    signature = c(dataset="ngchmDataset", where="character", covariate="ngchmCovariate"),
    definition = function (dataset, where, covariate) {
	if (!(where %in% c("row", "column", "both"))) {
	    stop (sprintf ("chmAddCovariate: unknown where '%s'. Should be row, column, or both.", where));
	}
	if (where %in% c("row", "both")) {
	    dataset@row.covariates <- append (dataset@row.covariates, covariate);
	}
	if (where %in% c("column", "both")) {
	    dataset@column.covariates <- append (dataset@column.covariates, covariate);
	}
        dataset
});

appendRendererIfNew <- function (colormaps, newmap) {
    for (cm in colormaps) {
        if (sameColormap (cm, newmap)) return (colormaps);
    }
    append (colormaps, newmap)
}

#' @rdname chmAddColormap-method
#' @aliases chmAddColormap,ngchm,ngchmColormap-method
setMethod ("chmAddColormap",
    signature = c(chm="ngchm", colormap="ngchmColormap"),
    definition = function (chm, colormap) {
        chm <- chmFixVersion (chm);
        chm@colormaps <- appendRendererIfNew (chm@colormaps, colormap);
        chmUU (chm)
});

#' @rdname chmAddRelatedGroup-method
#' @aliases chmAddRelatedGroup,ngchm,character,character,character,character-method
setMethod ("chmAddRelatedGroup",
    signature = c(chm="ngchm", name="character", header="character", linktype="character", blurb="character"),
    definition = function (chm, name, header, linktype, blurb) {
        chm <- chmFixVersion (chm);
	related <- new (Class="ngchmRelatedGroup", name=name, header=header, linktype=linktype, blurb=blurb);
	if ((length(chm@relatedGroups) + length(chm@relatedLinks)) == 0)
	    chm@extrafiles <- c(chm@extrafiles, "relatedlinks.js");
	chm@relatedGroups <- append (chm@relatedGroups, related);
        chmUU (chm)
});

#' @rdname chmAddRelatedGroup-method
#' @aliases chmAddRelatedGroup,ngchm,character,character,character,missing-method
setMethod ("chmAddRelatedGroup",
    signature = c(chm="ngchm", name="character", header="character", linktype="character", blurb="missing"),
    definition = function (chm, name, header, linktype) {
        chm <- chmFixVersion (chm);
	related <- new (Class="ngchmRelatedGroup", name=name, header=header, linktype=linktype, blurb=NULL);
	if ((length(chm@relatedGroups) + length(chm@relatedLinks)) == 0)
	    chm@extrafiles <- c(chm@extrafiles, "relatedlinks.js");
	chm@relatedGroups <- append (chm@relatedGroups, related);
        chmUU (chm)
});

#' @rdname chmAddRelated-method
#' @aliases chmAddRelated,ngchm,character,character,character-method
setMethod ("chmAddRelated",
    signature = c(chm="ngchm", group="character", link="character", description="character"),
    definition = function (chm, group, link, description) {
        chm <- chmFixVersion (chm);
	related <- new (Class="ngchmRelated", group=group, link=link, description=description);
	if ((length(chm@relatedGroups)+length(chm@relatedLinks)) == 0)
	    chm@extrafiles <- c(chm@extrafiles, "relatedlinks.js");
	chm@relatedLinks <- append (chm@relatedLinks, related);
        chmUU (chm)
});

#' @rdname chmAddOverview-method
#' @aliases chmAddOverview,ngchm,character,numeric,numeric-method
setMethod ("chmAddOverview",
    signature = c(chm="ngchm", format="character", width="optNumeric", height="optNumeric"),
    definition = function (chm, format, width, height) {
        chm <- chmFixVersion (chm);
	known.formats <- c("pdf", "png", "svg");
	if (length(format) != 1)
	    stop (sprintf ("chmAddOverview: format has length %d. Exactly one format string is required.", length(format)));
	if (!(format %in% known.formats))
	    stop (sprintf ("chmAddOverview: unknown overview format '%s'.  Acceptable formats are %s", format,
	                   paste (sprintf ("'%s'", known.formats), collapse=", ")));
	if (length(width) > 1)
	    stop (sprintf ("chmAddOverview: width has length %d. At most one width can be specified.", length(width)));
	if (length(height) > 1)
	    stop (sprintf ("chmAddOverview: height has length %d. At most one height can be specified.", length(height)));
	#if ((length(width) + length(height)) == 0)
	#    stop (sprintf ("chmAddOverview: at least width or height must be specified."));
	if (!is.null(width))
	    width <- as.integer(width);
	if (!is.null(height))
	    height <- as.integer(height);
	ov <- new (Class="ngchmOverview", format=format, width=width, height=height);
        chm@overviews <- append (chm@overviews, ov);
        chmUU (chm)
    });

#' @rdname chmAddTemplate-method
#' @aliases chmAddTemplate,ngchm,charOrFunction,character,optList-method
#'
setMethod ("chmAddTemplate",
    signature = c(chm="ngchm", source.path="charOrFunction", dest.path="character", substitutions="optList"),
    definition = function (chm, source.path, dest.path, substitutions) {
        chm <- chmFixVersion (chm);
    blob <- ngchmSaveTemplateAsBlob(ngchm.env$tmpShaidy, source.path, dest.path, substitutions);
	template <- new (Class="ngchmTemplate", source.path=source.path, dest.blob=blob, dest.path=dest.path, substitutions=substitutions);
	chm@extrafiles <- c (chm@extrafiles, dest.path);
	chm@templates <- append (chm@templates, template);
        chmUU (chm)
});

#' @rdname chmAddProperty-method
#' @aliases chmAddProperty,ngchm,character,character-method
#'
setMethod ("chmAddProperty",
    signature = c(chm="ngchm", label="character", value="character"),
    definition = function (chm, label, value) {
        chm <- chmFixVersion (chm);
	chm@properties <- append (chm@properties, new (Class="ngchmProperty", label=label, value=value));
        chmUU (chm)
});

#' @rdname chmAddSpecificAxisTypeFunction-method
#' @aliases chmAddSpecificAxisTypeFunction,ngchm,character,character,character,ngchmJS-method
#'
setMethod ("chmAddSpecificAxisTypeFunction",
    signature = c(chm="ngchm", where="character", type="character", label="character", func="ngchmJS"),
    definition = function (chm, where, type, label, func) {
        chm <- chmFixVersion (chm);
	af <- new ("ngchmAxisFunction", type=type, label=label, func=func);
	if ((length(where) != 1) || (! where %in% c("row", "column", "both"))) {
	    stop (sprintf ("chmAddSpecificAxisTypeFunction: unknown where '%s'. Should be row, column, or both.", where));
	}
	if ((where == "row") || (where == "both")) {
	    matches <- which (vapply (chm@rowTypeFunctions, function(af) (af@label == label) && (af@type == type), TRUE));
	    if (length (matches) > 0) {
		chm@rowTypeFunctions[[matches]] <- af;
	    } else {
		chm@rowTypeFunctions <- append (chm@rowTypeFunctions, af);
	    }
	}
	if ((where == "column") || (where == "both")) {
	    matches <- which (vapply (chm@colTypeFunctions, function(af) (af@label == label) && (af@type == type), TRUE));
	    if (length (matches) > 0) {
		chm@colTypeFunctions[[matches]] <- af;
	    } else {
		chm@colTypeFunctions <- append (chm@colTypeFunctions, af);
	    }
	}
	chmUU (chm)
    }
);

#' @rdname chmAddSpecificAxisTypeFunction-method
#' @aliases chmAddSpecificAxisTypeFunction,ngchm,character,character,character,character-method
#'
setMethod ("chmAddSpecificAxisTypeFunction",
    signature = c(chm="ngchm", where="character", type="character", label="character", func="character"),
    definition = function (chm, where, type, label, func) {
	chmAddSpecificAxisTypeFunction (chm, where, type, label, chmGetFunction (func))
    }
);

addFunDefine <- function (chm, func) {
    dup <- 0;
    if (is.list(chm@javascript)) {
	for (ii in 1:length(chm@javascript))
	    if (chm@javascript[[ii]]@name == func@name) {
		dup <- ii;
		if (chm@javascript[[ii]]@script != func@script)
		    stop (sprintf ("Duplicate definition of function '%s' differs from first definition", func@name));
	    }
    }
    if (dup == 0)
	chm@javascript = append (chm@javascript, func);
    chm
}

#' @rdname chmAddMenuItem-method
#' @aliases chmAddMenuItem,ngchm,character,character,ngchmJS-method
#'
setMethod ("chmAddMenuItem",
    signature = c(chm="ngchm", where="character", label="character", func="ngchmJS"),
    definition = function (chm, where, label, func) {
        chm <- chmFixVersion (chm);
	if (length(func@extraParams) > 0)
	    stop (sprintf ("Error adding menu item: function '%s' has unbound extra parameters", func@name));
	entry <- new (Class="ngchmMenuItem", label=label, description=func@description, fun=func@name);
	if (where == "row" || where == "both") {
	    chm@rowMenu <- append (chm@rowMenu, entry);
	    if (where == "both")
		chm@colMenu <- append (chm@colMenu, entry);
	} else if (where == "column") {
	    chm@colMenu <- append (chm@colMenu, entry);
	} else if (where == "element") {
	    chm@elementMenu <- append (chm@elementMenu, entry);
	} else if (where != "nowhere") {
	    stop (sprintf ("chmAddMenuItem: unknown where '%s'. Should be row, column, both, or element (or nowhere).", where));
	}
	chmUU (addFunDefine (chm, func))
});

#' @rdname chmAddMenuItem-method
#' @aliases chmAddMenuItem,ngchm,character,character,character-method
#'
setMethod ("chmAddMenuItem",
    signature = c(chm="ngchm", where="character", label="character", func="character"),
    definition = function (chm, where, label, func) {
        chmAddMenuItem (chm, where, label, chmGetFunction (func))
    }
);

#' @rdname chmAddAxisType-method
#' @aliases chmAddAxisType,ngchm,character,character,ngchmJS-method
#'
setMethod ("chmAddAxisType",
    signature = c(chm="ngchm", where="character", type="character", func="ngchmJS"),
    definition = function (chm, where, type, func) {
        chm <- chmFixVersion (chm);
        if (length(type) > 1) type <- paste (type, collapse='.bar.');
	at <- new (Class="ngchmAxisType", where=where, type=type, func=func);
	chm@axisTypes <- append (chm@axisTypes, at);
	chmAddProperty (chm, paste('!axistype', where, sep='.'), type)
    }
);

#' @rdname chmAddAxisType-method
#' @aliases chmAddAxisType,ngchm,character,character,character-method
#'
setMethod ("chmAddAxisType",
    signature = c(chm="ngchm", where="character", type="character", func="character"),
    definition = function (chm, where, type, func) {
        chmAddAxisType (chm, where, type, chmGetFunction (func))
    }
);

#' @rdname chmAddAxisType-method
#' @aliases chmAddAxisType,ngchm,character,character,missing-method
#'
setMethod ("chmAddAxisType",
    signature = c(chm="ngchm", where="character", type="character", func="missing"),
    definition = function (chm, where, type, func) {
        chmAddAxisType (chm, where, type, chmGetFunction ("getLabelValue"))
    }
);

#' @rdname chmAddCovariateBar-method
#' @aliases chmAddCovariateBar,ngchm,character,ngchmCovariateBar-method
#'
setMethod ("chmAddCovariateBar",
    signature = c(chm="ngchm", where="character", covar="ngchmBar"),
    definition = function (chm, where, covar) {
        chm <- chmFixVersion (chm);
	bar <- covar;
	validateNewCovariateBar (chm, where, bar);
	if (where == "row" || where == "both") {
	    chm@rowCovariateBars <- append (chm@rowCovariateBars, bar);
	    if (where == "both")
		chm@colCovariateBars <- append (chm@colCovariateBars, bar);
	} else if (where == "column") {
	    chm@colCovariateBars <- append (chm@colCovariateBars, bar);
	} else {
	    stop (sprintf ("chmAddCovariateBar: unknown where '%s'. Should be row, column, or both.", where));
	}
	chmUU (chm)
});
#' @rdname chmAddCovariateBar-method
#' @aliases chmAddCovariateBar,ngchm,character,ngchmCovariate-method
#'
setMethod ("chmAddCovariateBar",
    signature = c(chm="ngchm", where="character", covar="ngchmCovariate"),
    definition = function (chm, where, covar,
                           display="visible", thickness=as.integer(10), merge) {
	bar <- chmNewCovariateBar (covar, display=display, thickness=thickness, merge=merge);
	chmAddCovariateBar (chm, where, bar)
});
#' @rdname chmAddCovariateBar-method
#' @aliases chmAddCovariateBar,ngchm,character,list-method
#'
setMethod ("chmAddCovariateBar",
    signature = c(chm="ngchm", where="character", covar="list"),
    definition = function (chm, where, covar,
                           display="visible", thickness=as.integer(10), merge=NULL) {
        chm <- chmFixVersion (chm);
	for (item in covar) {
	    if (class(item) == "ngchmBar") {
	        bar <- item;
	    } else if (class(item) == "ngchmCovariate") {
		bar <- chmNewCovariateBar (item, display=display, thickness=thickness, merge=merge);
	    } else {
	        stop (sprintf ('adding unknown object of unknown class "%s"', class(item)));
	    }
	    chm <- chmAddCovariateBar (chm, where, bar)
	}
	chm
});


#' @rdname chmBindFunction-method
#' @aliases chmBindFunction,character,ngchmJS,list-method
setMethod ("chmBindFunction",
    signature = c(name="character", fn="ngchmJS", bindings="list"),
    definition = function (name, fn, bindings) {
	if (is.null (fn@extraParams) || (length(bindings) > length(fn@extraParams))) {
	    extra <- c();
	    if (!is.null (fn@extraParams)) extra <- fn@extraParams;
	    stop (sprintf ("chmBindFunction: %s more bindings (%d) than optional parameters (%d)", fn@name, length(bindings), length(extra)));
	}
	for (ii in 1:length(bindings)) {
	    if (names(bindings)[ii] != fn@extraParams[ii])
	        stop (sprintf ("binding name '%s' does not match corresponding parameter '%s'", names(bindings)[ii], fn@extraParams[ii]));
	}
	newdesc <- sprintf ("function %s bound to %d values", fn@name, length(bindings));
	params <- vapply(bindings, function(x) {
	    if (length(x) != 1) stop ("each parameter binding requires exact one value");
	    if (typeof(x)=="integer") { sprintf ("%d", x); }
	    else if (typeof(x)=="double") { sprintf ("%.10g",x); }
	    else if (typeof(x)=="logical") { c("false","true")[x+1];}
	    else if (typeof(x)=="character") { sprintf ("'%s'", x); }
	    else { stop ("unknown type of parameter binding"); }
	}, "");
	params <- paste (params, collapse=",");
	if (length(bindings) == length(fn@extraParams)) {
	    newextra <- NULL;
	} else {
	    newextra <- fn@extraParams[(1+length(bindings)):length(fn@extraParams)];
	}
	impl <- sprintf ("var %s = %s.bind (undefined, %s);", name, fn@name, params);
	chmNewFunction (name, newdesc, impl, extraParams=newextra, requires=c(fn@name), global=fn@global)
});

#' @rdname chmBindFunction-method
#' @aliases chmBindFunction,character,character,list-method
setMethod ("chmBindFunction",
    signature = c(name="character", fn="character", bindings="list"),
    definition = function (name, fn, bindings) {
	fndef <- chmGetFunction (fn);
	if (length(fndef) == 0)
	    stop (sprintf ("Unable to create binding '%s': function '%s' does not exist", name, fn));
        chmBindFunction (name, fndef, bindings)
});

orderMethod <- function(v) {
    if (length(v)==0) {
        return ("Original");
    }
    if (is(v, "function")) {
        if (identical(v,chmDefaultRowOrder) || identical(v,chmDefaultColOrder)) return ("Hierarchical");
        if (identical(v,chmRandomRowOrder) || identical(v,chmRandomColOrder)) return ("Random");
        if (identical(v,chmOriginalRowOrder) || identical(v,chmOriginalColOrder)) return ("Original");
    }
    return ("User");
}

#' @rdname chmRowOrder-method
#' @aliases chmRowOrder<-,ngchm,optDendrogram-method
setReplaceMethod ("chmRowOrder",
    signature = c(chm="ngchm", value="optDendrogram"),
    definition = function (chm, value) {
        chm <- chmFixVersion (chm);
	if (class(value) == "file") {
	    value <- readLines (value);
	    class(value) <- "fileContent";
	}
	chm@rowOrder <- value
	chm@rowOrderMethod <- orderMethod(value);
        chmUU (chm)
});

#' @rdname chmColOrder-method
#' @aliases chmColOrder<-,ngchm,optDendrogram-method
setReplaceMethod ("chmColOrder",
    signature = c(chm="ngchm", value="optDendrogram"),
    definition = function (chm, value) {
        chm <- chmFixVersion (chm);
	if (class(value) == "file") {
	    value <- readLines (value);
	    class(value) <- "fileContent";
	}
	chm@colOrder <- value
	chm@colOrderMethod <- orderMethod(value);
        chmUU (chm)
});

metaToShaid <- function (metadata) {
    stopifnot (!identical(names(metadata),NULL));
    metadata <- metadata[order(names(metadata))];
    mat <- matrix (metadata, ncol=1, dimnames=list(names(metadata),'Value'));
    shaid <- ngchmSaveAsDatasetBlob (ngchm.env$tmpShaidy, 'tsv', mat);
    shaid
}

#' @rdname chmAddMetaData-method
#' @aliases chmAddMetaData,ngchm,character,character,character-method
setMethod ("chmAddMetaData",
    signature = c(chm="ngchm", where="character", type="character", value="character"),
    definition = function (chm, where, type, value) {
        stopifnot(where %in% c("row","column","both"));
        chm <- chmFixVersion(chm);
        meta <- new('ngchmMetaData', type=type, value=metaToShaid(value));
        if (where %in% c('row','both')) {
	        chm@rowMeta <- append(chm@rowMeta, meta);
        }
        if (where %in% c('column','both')) {
	        chm@colMeta <- append(chm@colMeta, meta);
        }
        chmUU(chm)
});

make.js.names <- function (sss) {
    sss <- make.names (sss);
    vapply (sss, function(ss)gsub('.','_',ss,fixed=TRUE), "")
}

#' @rdname chmAddToolboxR-method
#' @aliases chmAddToolboxR,ngchm,character,character,character,character-method
setMethod ("chmAddToolboxR",
    signature = c(CHM="ngchm", axis="character", axistype="character", datasetname="character", idstr="character"),
    definition = function (CHM, axis, axistype, datasetname, idstr) {
        CHM <- chmFixVersion (CHM);
	toolbox <- ngchm.env$toolbox;
	if (length(toolbox)>0) {
	    for (ii in 1:nrow(toolbox)) {
		if (toolbox[ii,]$type == "R") {
		    fnname <- sprintf ("%s%s", toolbox[ii,]$fn@name, make.js.names(datasetname));
		    fndef <- chmGetFunction (fnname);
		    if (length(fndef) == 0) {
			chmBindFunction (fnname, toolbox[ii,]$fn@name, list(dataset=datasetname));
		    }
		    fnlabel = sprintf ("%s%s", toolbox[ii,]$label, idstr);
		    CHM <- chmAddSpecificAxisTypeFunction (CHM, axis, axistype, fnlabel, fnname);
		}
	    }
	}
	CHM
});

#' @rdname chmAddToolboxR2-method
#' @aliases chmAddToolboxR2,ngchm,character,character,character-method
setMethod ("chmAddToolboxR2",
    signature = c(CHM="ngchm", axistype="character", datasetname="character", idstr="character"),
    definition = function (CHM, axistype, datasetname, idstr) {
        CHM <- chmFixVersion (CHM);
	toolbox <- ngchm.env$toolbox;
	if (length(toolbox)>0) {
	    for (ii in 1:nrow(toolbox)) {
		if (toolbox[ii,]$type == "R2") {
		    fnname <- sprintf ("%s%s", toolbox[ii,]$fn@name, make.js.names(datasetname));
		    fndef <- chmGetFunction (fnname);
		    if (length(fndef) == 0) {
			chmBindFunction (fnname, toolbox[ii,]$fn@name, list(dataset=datasetname));
		    }
		    fnlabel = sprintf ("%s%s", toolbox[ii,]$label, idstr);
		    CHM <- chmAddMenuItem (CHM, "element", fnlabel, chmGetFunction(fnname));
		}
	    }
	}
	CHM
});

#' @rdname chmAddToolboxRC-method
#' @aliases chmAddToolboxRC,ngchm,character,character,character-method
setMethod ("chmAddToolboxRC",
    signature = c(CHM="ngchm", rowtype="character", coltype="character", datasetname="character", idstr="character"),
    definition = function (CHM, rowtype, coltype, datasetname, idstr) {
        CHM <- chmFixVersion (CHM);
	toolbox <- ngchm.env$toolbox;
	if (length(toolbox)>0) {
	    for (ii in 1:nrow(toolbox)) {
		if (toolbox[ii,]$type == "RC") {
		    fnname <- sprintf ("%s%s", toolbox[ii,]$fn@name, make.js.names(datasetname));
		    fndef <- chmGetFunction (fnname);
		    if (length(fndef) == 0) {
			chmBindFunction (fnname, toolbox[ii,]$fn@name, list(dataset=datasetname));
		    }
		    fnlabel = sprintf ("%s%s", toolbox[ii,]$label, idstr);
		    CHM <- chmAddMenuItem (CHM, "element", fnlabel, chmGetFunction(fnname));
		}
	    }
	}
	CHM
});

#' @rdname shaidyGetShaid-method
#' @aliases shaidyGetShaid,ngchm-method
setMethod ("shaidyGetShaid",
    signature = c(object="ngchm"),
    definition = function(object) {
        ngchmSaveChmAsBlob (ngchm.env$tmpShaidy, object)
});

#' @rdname shaidyGetComponents-method
#' @aliases shaidyGetComponents,ngchm-method
setMethod ("shaidyGetComponents",
    signature = c(object="ngchm"),
    definition = function(object) {
        if (is(object@rowOrder,"function")) object@rowOrder <- object@rowOrder (object);
        if (is(object@colOrder,"function")) object@colOrder <- object@colOrder (object);
        unique(c(object@rowOrder, object@colOrder,
          if (is(object@rowOrder,"shaid") && object@rowOrder@type=='dendrogram') ngchmGetLabels(object@rowOrder)[[1]] else NULL,
          if (is(object@colOrder,"shaid") && object@colOrder@type=='dendrogram') ngchmGetLabels(object@colOrder)[[1]] else NULL,
          lapply(object@layers,function(x)x@data),
          lapply(object@colCovariateBars,function(x)x@data),
          lapply(object@rowCovariateBars,function(x)x@data),
          lapply(object@templates,function(x)x@dest.blob),
          lapply(object@rowMeta, function(x)x@value),
          lapply(object@colMeta, function(x)x@value),
          lapply(object@datasets, shaidyGetComponents),
          recursive=TRUE
          ))
});

#' @rdname shaidyGetComponents-method
#' @aliases shaidyGetComponents,ngchmDataset-method
setMethod ("shaidyGetComponents",
    signature = c(object="ngchmDataset"),
    definition = function(object) {
        unique(c(object@data,
          lapply(object@row.covariates, shaidyGetComponents),
          lapply(object@column.covariates, shaidyGetComponents),
          recursive=TRUE
        ))
    });
#' @rdname shaidyGetComponents-method
#' @aliases shaidyGetComponents,ngchmCovariate-method
setMethod ("shaidyGetComponents",
    signature = c(object="ngchmCovariate"),
    definition = function(object) {
        object@label.series
    });

#' @rdname chmGetDataset-method
#' @aliases chmGetDataset,ngchmLayer-method
setMethod ("chmGetDataset",
    signature = c(object="ngchmLayer"),
    definition = function(object) {
        shaid <- object@data;
        repo <- ngchmFindRepo (shaid);
        ngchmLoadDatasetBlob (repo, shaid)
});

#' @rdname chmHasProperty-method
#' @aliases chmHasProperty,ngchmVersion2-method
setMethod ("chmHasProperty",
    signature = c(object="ngchmVersion2", label="character"),
    definition = function(object,label) {
        matches <- vapply (object@properties, function(p) p@label==label, rep(TRUE,length(label)));
        if (length(label)==1) any (matches) else apply (matches, 1, any)
    }
);

#' @rdname chmGetProperty-method
#' @aliases chmGetProperty,ngchmVersion2-method
setMethod ("chmGetProperty",
    signature = c(object="ngchmVersion2", label="character"),
    definition = function(object,label) {
        stopifnot (length(label) == 1);
        matches <- vapply (object@properties, function(p) p@label==label, TRUE);
        vapply (object@properties[matches], function(p) p@value, '')
    }
);
