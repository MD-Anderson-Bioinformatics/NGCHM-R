
systemCheck <- function (command) {
    # Execute the specified command and halt execution with an error
    # message if it fails.
    status <- system (command)
    if (status != 0) {
        stop ('Error encountered executing system command: ', command)
    }
    status
}

# ##############################################################################################
#
# Methods for class NGCHM.SERVER
#

#' @rdname chmDeployServer-method
#' @aliases chmDeployServer,ngchmServer-method
#'
setMethod ("chmDeployServer",
    signature = c(server="ngchmServer"),
    definition = function (server) server@deployServer);

#' @rdname chmDeployDir-method
#' @aliases chmDeployDir,ngchmServer-method
#'
setMethod ("chmDeployDir",
    signature = c(server="ngchmServer"),
    definition = function (server) server@deployDir);

#' @rdname chmUrlBase-method
#' @aliases chmUrlBase,ngchmServer-method
#'
setMethod ("chmUrlBase",
    signature = c(server="ngchmServer"),
    definition = function (server) server@urlBase);


getServerDest <- function (server) {
    dest <- chmDeployServer(server);
    if (!is.null(server@username)) { dest <- sprintf ("%s@%s", server@username, dest); }
    dest <- shQuote (dest);
    if (!is.null (server@keypath)) { dest <- sprintf ("-i %s %s", shQuote (server@keypath), dest); }
    dest
}

#' @rdname chmInstall-method
#' @aliases chmInstall,ngchmServer,ngchm-method
setMethod ("chmInstall",
    signature = c(server="ngchmServer", chm="ngchm"),
    definition = function (server, chm) {
	server@serverProtocol@installMethod (server, chm);
    });

#' @rdname chmUninstall-method
#' @aliases chmUninstall,ngchmServer,ngchm-method
setMethod ("chmUninstall",
    signature = c(server="ngchmServer", chm="ngchm"),
    definition = function (server, chm) {
        chmUninstall (server, chmName(chm));
});

#' @rdname chmUninstall-method
#' @aliases chmUninstall,ngchmServer,character-method
setMethod ("chmUninstall",
    signature = c(server="ngchmServer", chm="character"),
    definition = function (server, chm) {
	server@serverProtocol@uninstallMethod (server, chm);
    });

#' @rdname chmMakePrivate-method
#' @aliases chmMakePrivate,ngchmServer,ngchm-method
setMethod ("chmMakePrivate",
    signature = c(server="ngchmServer", chm="ngchm"),
    definition = function (server, chm) {
        chmMakePrivate (server, chmName(chm));
});

#' @rdname chmMakePrivate-method
#' @aliases chmMakePrivate,ngchmServer,character-method
setMethod ("chmMakePrivate",
    signature = c(server="ngchmServer", chm="character"),
    definition = function (server, chm) {
	server@serverProtocol@makePrivate (server, chm);
    });

#' @rdname chmMakePublic-method
#' @aliases chmMakePublic,ngchmServer,ngchm-method
setMethod ("chmMakePublic",
    signature = c(server="ngchmServer", chm="ngchm"),
    definition = function (server, chm) {
        chmMakePublic (server, chmName(chm));
});

#' @rdname chmMakePublic-method
#' @aliases chmMakePublic,ngchmServer,character-method
setMethod ("chmMakePublic",
    signature = c(server="ngchmServer", chm="character"),
    definition = function (server, chm) {
	server@serverProtocol@makePublic (server, chm);
    });

# ##############################################################################################
#
# Methods for class NGCHM
#

writeColorMap <- function (context, cmap, prefix, suffix, chan) {
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

writeMenu <- function (menu, prefix, chan) {
    if (is.list(menu)) {
	for (ii in 1:length(menu)) {
	    cat (sprintf ("    chm.%s.addMenuItem ('%s', %s)\n", prefix, menu[[ii]]@label, menu[[ii]]@fun), file=chan);
	}
    }
}

writeCSS <- function (css, inpDir) {
    chan <- file (paste (inpDir, "custom.css", sep="/"), "w");
    for (ii in 1:length(css))
        cat (css[[ii]]@css, sep="\n", file=chan);
    close (chan);
}

writeProperties <- function (inpDir, props, chan) {
    for (ii in 1:length(props)) {
	if (props[[ii]]@label != "hidden") {
	    cat (sprintf ("%s=%s\n", props[[ii]]@label, props[[ii]]@value), file=chan);
	}
    }
}

writePropertiesPost <- function (outDir, props) {
    for (ii in 1:length(props)) {
	if ((props[[ii]]@label == "hidden") && (props[[ii]]@value == "TRUE")) {
	    cat ("", file=file.path (outDir, "hidden.txt"));
	}
    }
}

writeChmPost <- function (chm) {
    if (is.list(chm@properties)) writePropertiesPost (file.path(chm@outDir,chm@name), chm@properties);
}

startcust <- paste ("(function(chm){",
	       "chm.chmv = '/chmv/';",
               "function _chm_e(sr,ax,fn){function c2(a,b){return a.concat(b);};",
               "return sr.map(function(r){var v=[];for(var ii=r.start;ii<=r.end;ii++)v.push(ii);",
	       "return v.map(function(i){return fn(ax,i);}).reduce(c2);}).reduce(c2);}\n", sep="\n");

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

writeCustomJS <- function (chm) {
    rqJSfuns <- requiredFunctions (list(), chm@javascript);
    chan <- file (paste (chm@inpDir, "custom.js", sep="/"), "w");
    if (length(rqJSfuns) > 0) writeJS (rqJSfuns, chan, TRUE);
    cat (startcust, file=chan);
    if (length(rqJSfuns) > 0) writeJS (rqJSfuns, chan, FALSE);
    cat ("chm.addCustomization(function(){\n", file=chan);
    writeMenu (chm@rowMenu, "row.labels", chan);
    writeMenu (chm@rowMenu, "row.dendrogram", chan);
    writeMenu (chm@colMenu, "column.labels", chan);
    writeMenu (chm@colMenu, "column.dendrogram", chan);
    writeMenu (chm@elementMenu, "matrix", chan);
    cat ("});\n", file=chan);
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
    if (cmap1@missing != cmap2@missing)
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
    write.table (layer@data, file=paste (dir, sprintf("%s.data.tsv", prefix), sep="/"),
                 sep="\t", quote=FALSE);
}

writeClassBar <- function (cbar, inpDir, type, index, chan) {
    cat (sprintf ("classification.type%d=%s\n", index, cbar@type), file=chan);
    cat (sprintf ("classification.label%d=%s\n", index, cbar@label), file=chan);
    cat (sprintf ("classification.display%d=%s\n", index, cbar@display), file=chan);
    cat (sprintf ("classification.thickness%d=%d\n", index, cbar@thickness), file=chan);
    if (length (cbar@merge) > 0)
	cat (sprintf ("classification.mergingAlgorithm%d=%s\n", index, cbar@merge), file=chan);
    if (length (cbar@colors) > 0)
	writeColorMap ("class", cbar@colors, "classification", sprintf ("%d", index), chan);

    chan2 <- file (paste (inpDir, sprintf ("%sClassificationData%d.txt", type, index), sep="/"), "w")
    nm <- names(cbar@data)
    for (ii in 1:length(cbar@data))
        cat (nm[ii], "\t", cbar@data[ii], "\n", sep="", file=chan2);
    close (chan2);
}


writeDataset <- function (chm, dataset, dir) {
    library (tsvio);
    chm@extrafiles <- c(chm@extrafiles, sprintf ("%s.tsv", dataset@name));
    chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-index.tsv", dataset@name));

    write.table (dataset@data, file.path (dir, sprintf ("%s.tsv", dataset@name)), sep="\t", quote=FALSE);
    tsvGenIndex (file.path (dir, sprintf ("%s.tsv", dataset@name)),
                 file.path (dir, sprintf ("%s-index.tsv", dataset@name)));

    all.covariates = unique (append (dataset@row.covariates, dataset@column.covariates));
    if (length(all.covariates) > 0) {
	chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-covariates.tsv", dataset@name));
	cov.table <- list(Covariate=vapply(all.covariates, function(cov)cov@label, ""),
	                  Fullname=vapply(all.covariates, function(cov)cov@fullname, ""));
	write.table(cov.table,
		    file.path (dir, sprintf ("%s-covariates.tsv", dataset@name)),
		    sep="\t", quote=FALSE, row.names=FALSE);
    }

    if (length(dataset@row.covariates) > 0) {
	first.rowser <- TRUE;
	first.serprop <- TRUE;
	for (ii in 1:length(dataset@row.covariates)) {
	    cov <- dataset@row.covariates[[ii]];
	    rowser <- list (Sample=names(cov@label.series), Series=cov@label.series, Covariate=rep(cov@label,length(cov@label.series)));
	    if (first.rowser) {
	        first.rowser <- FALSE;
		chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-row-series.tsv", dataset@name));
		fd.rowser <- file (file.path (dir, sprintf ("%s-row-series.tsv", dataset@name)), "w");
		write.table(rowser, file=fd.rowser, sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE);
	    } else {
		write.table(rowser, file=fd.rowser, sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE);
	    }
	    if (length(cov@series.properties) > 0) {
		serprop <- append (list(Covariate=rep(cov@label,length(cov@series.properties[[1]]))), cov@series.properties);
		if (first.serprop) {
		    first.serprop <- FALSE;
		    chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-rseries-properties.tsv", dataset@name));
		    fd.serprop <- file (file.path (dir, sprintf ("%s-rseries-properties.tsv", dataset@name)), "w");
		    write.table(serprop, file=fd.serprop, sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE);
		} else {
		    write.table(serprop, file=fd.serprop, sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE);
		}
	    }
	}
	if (!first.rowser) close (fd.rowser);
	if (!first.serprop) close (fd.serprop);
    }

    if (length(dataset@column.covariates) > 0) {
	first.colser <- TRUE;
	first.serprop <- TRUE;
	for (ii in 1:length(dataset@column.covariates)) {
	    cov <- dataset@column.covariates[[ii]];
	    colser <- list (Sample=names(cov@label.series), Series=cov@label.series, Covariate=rep(cov@label,length(cov@label.series)));
	    if (first.colser) {
	        first.colser <- FALSE;
		chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-sample-series.tsv", dataset@name));
		fd.colser <- file (file.path (dir, sprintf ("%s-sample-series.tsv", dataset@name)), "w");
		write.table(colser, file=fd.colser, sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE);
	    } else {
		write.table(colser, file=fd.colser, sep="\t", quote=FALSE, row.names=FALSE, col.names=FALSE);
	    }
	    if (length(cov@series.properties) > 0) {
		serprop <- append (list(Covariate=rep(cov@label,length(cov@series.properties[[1]]))), cov@series.properties);
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

writeTemplate <- function (template, outDir) {
    if ((class(template@source.path)=="character") && (length(template@substitutions) == 0)) {
	if (!file.copy (template@source.path, file.path (outDir, template@dest.path))) {
	    stop (sprintf ("Unable to copy template file '%s' to '%s'", template@source.path,
	                   file.path (outDir, template@dest.path)));
	}
        #systemCheck (sprintf ("/bin/cp %s %s",
	#                       shQuote (template@source.path),
	#		       shQuote (file.path (outDir, template@dest.path))));
    } else {
	if (class(template@source.path)=="character") {
	    data <- readLines (template@source.path);
	} else {
	    data <- template@source.path ();
	}
	for (ss in template@substitutions)
	    data <- gsub (ss[1], ss[2], data);
	writeLines (data, con=file.path (outDir, template@dest.path));
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

writeChmExtraSupport <- function (chm)
{
    #chmOutDir <- file.path (chm@outDir, chm@name);
    chmOutDir <- file.path (chm@inpDir);
    if ((length(chm@relatedLinks)+length(chm@relatedGroups)) > 0) {
	writeRelated (chm@relatedGroups, chm@relatedLinks, chmOutDir);
    }
    if (length(chm@datasets) > 0) {
	chan <- file (file.path (chmOutDir, "datasets.tsv"), "w");
	writeLines ("Dataset\tDescription", con=chan);
	for (ii in 1:length(chm@datasets)) {
	    ds <- chm@datasets[[ii]];
	    chm <- writeDataset (chm, ds, chmOutDir);
	    writeLines (sprintf ("%s\t%s", ds@name, ds@description), con=chan);
	}
	close (chan);
    }
    if (length(chm@templates) > 0) {
	for (ii in 1:length(chm@templates))
	    writeTemplate (chm@templates[[ii]], chmOutDir);
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
        return (sprintf ("%s(%s)", b@func@name, getValueExpr(tflist, b@fromtype, where)));
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

writeChm <- function (chm) {
    if (length (chm@layers) == 0)
        stop ("The NGCHM has no data layers. You must add at least one.");
    if (length (chm@colormaps) == 0)
        stop ("Internal error detected: the NGCHM has no color maps.  Please report.");

    # Add row menu items for types we know about.
    genSpecFeedback (10, "adding row menu items");
    rowtypes <- getAllAxisTypes (chm, "row");
    rowTypeFnsReqd <- rep(FALSE,length(rowtypes));
    rowfns <- getAllAxisTypeFunctions (chm@rowTypeFunctions, rowtypes$types);
    if (length(rowfns) > 0)
        for (ii in 1:length(rowfns)) {
	    fn <- rowfns[[ii]];
	    rowTypeFnsReqd[getFnsRqrd(rowtypes,fn@type)] <- TRUE;
	    entry <- new (Class="ngchmMenuItem", label=fn@label, description=fn@func@description,
	        fun = sprintf ("function(s,a,e){%s(%s);}", fn@func@name, getValueExpr(rowtypes,fn@type,"axis")));
	    chm@rowMenu <- append (chm@rowMenu, entry);
	}

    # Add column menu items for types we know about.
    genSpecFeedback (20, "adding column menu items");
    coltypes <- getAllAxisTypes (chm, "column");
    colTypeFnsReqd <- rep(FALSE,length(coltypes));
    colfns <- getAllAxisTypeFunctions (chm@colTypeFunctions, coltypes$types);
    if (length(colfns) > 0)
        for (ii in 1:length(colfns)) {
	    fn <- colfns[[ii]];
	    colTypeFnsReqd[getFnsRqrd(coltypes,fn@type)] <- TRUE;
	    entry <- new (Class="ngchmMenuItem", label=fn@label, description=fn@func@description,
	        fun = sprintf ("function(s,a,e){%s(%s);}", fn@func@name, getValueExpr(coltypes,fn@type,"axis")));
	    chm@colMenu <- append (chm@colMenu, entry);
	}

    # Add matrix element menu items for types we know about.
    genSpecFeedback (30, "adding matrix menu items");
    matfns <- getAllMatrixTypeFunctions (chm, rowtypes$types, coltypes$types);
    if (length(matfns) > 0)
        for (ii in 1:length(matfns)) {
	    fn <- matfns[[ii]];
	    rowTypeFnsReqd[getFnsRqrd(rowtypes,fn@rowtype)] <- TRUE;
	    colTypeFnsReqd[getFnsRqrd(coltypes,fn@columntype)] <- TRUE;
	    entry <- new (Class="ngchmMenuItem", label=fn@label, description=fn@func@description,
	        fun = sprintf ("function(rs,cs,e){%s(%s,%s);}", fn@func@name,
		               getValueExpr(rowtypes,fn@rowtype,"row"), getValueExpr(coltypes,fn@columntype,"column")));
	    chm@elementMenu <- append (chm@elementMenu, entry);
	    cat ("chmMake: added elementMenu entry ", entry@label, "\n", file=stderr());
	}

    # Add functions for getting type values from selections.
    genSpecFeedback (40, "adding type selector functions");
    cat ("chmMake: matfns contains ", length(matfns), " entries\n", file=stderr());
    fns <- append (matfns, unique (append (rowfns, colfns)));
    if (length(fns) > 0)
         for (ii in 1:length(fns)) {
	     if (length(fns[[ii]]) == 0) {
		cat ("chmMake: axis/mat fn entry is NULL\n", file=stderr());
	     } else {
		 chm <- chmAddMenuItem (chm, "nowhere", "unused", fns[[ii]]@func);
	     }
	 }

    fns <- unique (append (rowtypes$builders[rowTypeFnsReqd], coltypes$builders[colTypeFnsReqd]));
    if (length(fns) > 0)
         for (ii in 1:length(fns))
	     if (length(fns[[ii]]) == 0) {
		cat ("chmMake: builders fn entry is NULL\n", file=stderr());
	     } else {
		 chm <- chmAddMenuItem (chm, "nowhere", "unused", fns[[ii]]@func);
	     }

    genSpecFeedback (50, "creating specification directory");
    unlink (chm@inpDir, recursive=TRUE);
    if (!dir.create (chm@inpDir, recursive=TRUE)) {
        stop (sprintf ("Unable to create directory '%s' in which to save CHM specification", chm@inpDir));
    }
    #system (sprintf ("/bin/rm -rf %s", chm@inpDir));
    #systemCheck (sprintf ("/bin/mkdir %s", chm@inpDir));

    genSpecFeedback (60, "writing specification");
    props = file (chm@propFile, "w");
    cat (sprintf ("# This NGCHM property description was produced using the R NGCHM library version %s at %s\n",
                  packageDescription("NGCHM")$Version, date()), file=props);
    cat (sprintf ("data.set.name=%s\n", chm@name), file=props);
    cat (sprintf ("chm.main.image.height=%d\n", chm@height), file=props);
    cat (sprintf ("chm.main.image.width=%d\n", chm@width), file=props);
    if (length (chm@tags) > 0)
        cat (sprintf ("tags=%s\n", paste(chm@tags,sep=",",collapse=",")), file=props);
    genSpecFeedback (65, "writing color schemes");
    for (ii in 1:length(chm@colormaps))
	writeColorMap ("main", chm@colormaps[[ii]], sprintf("colormap%d", ii), "", props);
    genSpecFeedback (70, "writing data layers");
    for (ii in 1:length(chm@layers))
        writeDataLayer (chm, chm@layers[[ii]], chm@inpDir, ii, props);
    if (is.list(chm@properties)) writeProperties (chm@inpDir, chm@properties, props);
    if (is.list(chm@overviews)) {
	for (ii in 1:length(chm@overviews)) {
	    ov <- chm@overviews[[ii]];
	    cat (sprintf ("overview%d.format=%s\n", ii, ov@format));
	    if (length(ov@width) > 0)
		cat (sprintf ("overview%d.width=%d\n", ii, ov@width));
	    if (length(ov@height) > 0)
		cat (sprintf ("overview%d.height=%d\n", ii, ov@height));
	}
    }
    genSpecFeedback (80, "writing extra support files");
    chm <- writeChmExtraSupport (chm);
    if (length (chm@extrafiles) > 0)
        cat (sprintf ("additional.input=%s\n", paste(chm@extrafiles,sep="",collapse=",")), file=props);
    close (props);

    genSpecFeedback (90, "writing covariate bar data");
    if (!is.null(chm@rowOrder))
        writeOrder (chm@inpDir, "row", chm@rowOrder);
    if (!is.null(chm@colOrder))
        writeOrder (chm@inpDir, "column", chm@colOrder);
    if (!is.null(chm@rowMeta))
        writeMeta (chm@inpDir, "row", chm@rowMeta);
    if (!is.null(chm@colMeta))
        writeMeta (chm@inpDir, "column", chm@colMeta);
    if (is.list (chm@rowClassbars)) {
	chan = file (paste (chm@inpDir, "rowClassification1.txt", sep="/"), "w");
	for (ii in 1:length(chm@rowClassbars) )
	    writeClassBar (chm@rowClassbars[[ii]], chm@inpDir, "row", ii, chan);
	close (chan);
    }
    if (is.list(chm@colClassbars)) {
	chan = file (paste (chm@inpDir, "columnClassification1.txt", sep="/"), "w");
	for (ii in 1:length(chm@colClassbars))
	    writeClassBar (chm@colClassbars[[ii]], chm@inpDir, "column", ii, chan);
	close (chan);
    }

    genSpecFeedback (95, "writing custom javascript functions");
    if (is.list(chm@css)) writeCSS (chm@css, chm@inpDir);
    writeCustomJS (chm);
}

#' @rdname chmName-method
#' @aliases chmName,ngchm-method
#'
setMethod ("chmName",
           signature = c(chm="ngchm"),
	   definition = function (chm) chm@name);

writeOrder <- function (inpDir, type, ord) {
    # Write the order/dendrogram out as a column dendrogram to the inpDir
    if (class (ord) == "character") {
	filename = sprintf ("%s/%s.txt", inpDir, type);
        write.table (ord, filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
    } else if ((class (ord) == "dendrogram") || (class (ord) == "hclust")) {
	sink (paste (inpDir, sprintf ("dendro_%s.str", type), sep="/"))
	if (class (ord) == "hclust")
	    ord = as.dendrogram (ord);
	str (ord)
	sink (NULL)
    } else if (class (ord) == "fileContent") {
	filename = (paste (inpDir, sprintf ("dendro_%s.str", type), sep="/"));
	ff = file (filename, "w");
	writeLines (ord, ff);
	close (ff);
    } else if (class (ord) == "file") {
	stop ("Internal error detected: axis order type file should not be here. Please report.");
	filename = (paste (inpDir, sprintf ("dendro_%s.str", type), sep="/"));
	content <- readLines (ord);
	ff = file (filename, "w");
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
    if (class (metadata) == "character" || class(metadata) == "list") {
	filename = sprintf ("%s/%s_meta.txt", inpDir, type);
        write.table (metadata, filename, quote=FALSE, row.names=FALSE, col.names=TRUE, sep="\t")
    } else if (class (metadata) == "file") {
	filename = (paste (inpDir, sprintf ("%s_meta.txt", type), sep="/"));
	content <- readLines (metadata);
	ff = file (filename, "w");
	writeLines (content, ff);
	close (ff);
    } else if (class (metadata) == "NULL") {
        # Do nothing.
    } else {
        stop (sprintf ("chmWriteMeta: unknown class of %s metadata: '%s'", type, class(metadata)));
    }
}

#' @rdname chmGetURL-method
#' @aliases chmGetURL,ngchmServer,character-method
setMethod ("chmGetURL",
    signature = c(server="ngchmServer", chm="character"),
    definition = function (server, chm) {
        sprintf ("%s?name=%s", server@urlBase, chm)
});

#' @rdname chmGetURL-method
#' @aliases chmGetURL,ngchmServer,ngchm-method
setMethod ("chmGetURL",
    signature = c(server="ngchmServer", chm="ngchm"),
    definition = function (server, chm) {
        sprintf ("%s?name=%s", server@urlBase, chmName (chm))
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

#' @rdname chmMake-method
#' @aliases chmMake,ngchmServer,ngchm-method
#'
setMethod ("chmMake",
    signature = c(server="ngchmServer", chm="ngchm"),
    definition = function (server, chm, deleteOld=TRUE, useJAR=NULL, buildArchive=TRUE, javaTraceLevel="PROGRESS") {
    genSpecFeedback (0, "writing NGCHM specification");
    writeChm (chm);
    genSpecFeedback (100, "rendering NGCHM");

    dir.create (chm@outDir, recursive=TRUE, showWarnings=FALSE);
    unlink (file.path (chm@outDir, chm@name), recursive=TRUE);
    #system (sprintf ("/bin/mkdir -p %s", shQuote (chm@outDir)));
    #system (sprintf ("/bin/rm -rf %s/%s", shQuote (chm@outDir), shQuote(chm@name)));
    cat ("chmMake: getting builder JAR\n", file=stderr());
    if (length(useJAR) == 0) {
	if (length(grep("^scp://", server@jarFile)) > 0) {
	    parts <- URLparts (server@jarFile);
	    if (parts[3] == "") {
		systemCheck (sprintf ("scp %s:%s heatmappipeline.jar",
			      shQuote (parts[2]), shQuote(parts[4])));
	    } else {
		systemCheck (sprintf ("scp -P %s %s:%s heatmappipeline.jar",
			      shQuote(parts[3]), shQuote (parts[2]), shQuote(parts[4])));
	    }
	}
	else if (length(grep("^http://", server@jarFile)) > 0) {
	    systemCheck (sprintf ("wget -q -O heatmappipeline.jar %s",
			  shQuote (server@jarFile)));
	} else {
	    stop (sprintf ("chmMake: unknown retrieval method for jarFile %s", chm@jarFile));
	}
	useJAR = "heatmappipeline.jar";
    }
    cat ("chmMake: initiating Java process\n", file=stderr());
    javaTraceOpts <- ""
    if ((length(javaTraceLevel) > 0) && (length(server@traceLevel)>0)) {
	javaTraceOpts <- sprintf ("-l %s -p", shQuote(javaTraceLevel));
    }
    systemCheck (sprintf ("java -Djava.awt.headless=true -jar %s %s %s %s %s",
		  shQuote (useJAR),
		  javaTraceOpts,
		  shQuote (chm@inpDir),
		  shQuote (chm@propFile),
		  shQuote (chm@outDir)));
    cat ("chmMake: Java process completed\n", file=stderr());

    postBuildFeedback (0, "writing post build files");
    writeChmPost (chm);
    if (buildArchive) {
	if (Sys.info()[['sysname']] != "Windows")  {
	    postBuildFeedback (50, "creating compressed NGCHM file");
	    systemCheck (sprintf ("tar czf %s.ngchm.gz -C %s %s",
				 shQuote (chm@name),
				 shQuote (chm@outDir),
				 shQuote (chm@name)));
	}
    }
    postBuildFeedback (100, "post build completed");
});

#' @rdname chmMake-method
#' @aliases chmMake,ngchmServer,ngchm,missing-method
#'
#setMethod ("chmMake",
#    signature = c(server="ngchmServer", chm="ngchm", deleteOld="missing"),
#    definition = function (server, chm, deleteOld) {
#        chmMake (server, chm, TRUE);
#});

#' @rdname chmAddLayer-method
#' @aliases chmAddLayer,ngchm,ngchmLayer-method
#'
setMethod ("chmAddLayer",
    signature = c(chm="ngchm", layer="ngchmLayer"),
    definition = function (chm, layer) {
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
	chm@css <- append (chm@css, new (Class="ngchmCSS", css=css));
        chm
});

#' @rdname chmAddTag-method
#' @aliases chmAddTag,ngchm,character,character-method
setMethod ("chmAddTag",
    signature = c(chm="ngchm", tag="character"),
    definition = function (chm, tag) {
	chm@tags <- c (chm@tags, tag);
        chm
});

#' @rdname chmAddDataset-method
#' @aliases chmAddDataset,ngchm,ngchmDataset-method
setMethod ("chmAddDataset",
    signature = c(chm="ngchm", dataset="ngchmDataset"),
    definition = function (chm, dataset) {
	if (length(chm@datasets) == 0) {
	    chm@extrafiles <- c(chm@extrafiles, "datasets.tsv");
	}
	chm@datasets <- append (chm@datasets, dataset);
        chm
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

#' @rdname chmAddColormap-method
#' @aliases chmAddColormap,ngchm,ngchmColormap-method
setMethod ("chmAddColormap",
    signature = c(chm="ngchm", colormap="ngchmColormap"),
    definition = function (chm, colormap) {
	found <- FALSE
	if (length(chm@colormaps) > 0) {
	    for (ii in 1:length(chm@colormaps)) {
	        if (sameColormap (chm@colormaps[[ii]], colormap)) {
		    found = TRUE;
		    break;
		}
	    }
	}
	if (!found)
	    chm@colormaps <- append (chm@colormaps, colormap);
        chm
});

#' @rdname chmAddRelatedGroup-method
#' @aliases chmAddRelatedGroup,ngchm,character,character,character,character-method
setMethod ("chmAddRelatedGroup",
    signature = c(chm="ngchm", name="character", header="character", linktype="character", blurb="character"),
    definition = function (chm, name, header, linktype, blurb) {
	related <- new (Class="ngchmRelatedGroup", name=name, header=header, linktype=linktype, blurb=blurb);
	if ((length(chm@relatedGroups) + length(chm@relatedLinks)) == 0)
	    chm@extrafiles <- c(chm@extrafiles, "relatedlinks.js");
	chm@relatedGroups <- append (chm@relatedGroups, related);
        chm
});

#' @rdname chmAddRelatedGroup-method
#' @aliases chmAddRelatedGroup,ngchm,character,character,character,missing-method
setMethod ("chmAddRelatedGroup",
    signature = c(chm="ngchm", name="character", header="character", linktype="character", blurb="missing"),
    definition = function (chm, name, header, linktype) {
	related <- new (Class="ngchmRelatedGroup", name=name, header=header, linktype=linktype, blurb=NULL);
	if ((length(chm@relatedGroups) + length(chm@relatedLinks)) == 0)
	    chm@extrafiles <- c(chm@extrafiles, "relatedlinks.js");
	chm@relatedGroups <- append (chm@relatedGroups, related);
        chm
});

#' @rdname chmAddRelated-method
#' @aliases chmAddRelated,ngchm,character,character,character-method
setMethod ("chmAddRelated",
    signature = c(chm="ngchm", group="character", link="character", description="character"),
    definition = function (chm, group, link, description) {
	related <- new (Class="ngchmRelated", group=group, link=link, description=description);
	if ((length(chm@relatedGroups)+length(chm@relatedLinks)) == 0)
	    chm@extrafiles <- c(chm@extrafiles, "relatedlinks.js");
	chm@relatedLinks <- append (chm@relatedLinks, related);
        chm
});

#' @rdname chmAddOverview-method
#' @aliases chmAddOverview,ngchm,character,numeric,numeric-method
setMethod ("chmAddOverview",
    signature = c(chm="ngchm", format="character", width="numeric", height="numeric"),
    definition = function (chm, format, width, height) {
	if (length(format) != 1)
	    stop (sprintf ("chmAddOverview: format has length %d. Exactly one format string is required.", length(format)));
	if ((format != "pdf") && (format != "png"))
	    stop (sprintf ("chmAddOverview: unknown overview format '%s'.  Acceptable formats are 'pdf', 'png'", format));
	if (length(width) > 1)
	    stop (sprintf ("chmAddOverview: width has length %d. At most one width can be specified.", length(width)));
	if (length(height) > 1)
	    stop (sprintf ("chmAddOverview: height has length %d. At most one height can be specified.", length(height)));
	if ((length(width) + length(height)) == 0)
	    stop (sprintf ("chmAddOverview: at least width or height must be specified."));
	if (length(width) == 1)
	    width <- as.integer(width);
	if (length(height) == 1)
	    height <- as.integer(height);
	ov <- new (Class="ngchmOverview", format=format, width=width, height=height);
        chm@overviews <- append (chm@overviews, ov);
        chm
    });

#' @rdname chmAddTemplate-method
#' @aliases chmAddTemplate,ngchm,charOrFunction,character,optList-method
#'
setMethod ("chmAddTemplate",
    signature = c(chm="ngchm", source.path="charOrFunction", dest.path="character", substitutions="optList"),
    definition = function (chm, source.path, dest.path, substitutions) {
	template <- new (Class="ngchmTemplate", source.path=source.path, dest.path=dest.path, substitutions=substitutions);
	chm@extrafiles <- c (chm@extrafiles, dest.path);
	chm@templates <- append (chm@templates, template);
        chm
});

#' @rdname chmAddProperty-method
#' @aliases chmAddProperty,ngchm,character,character-method
#'
setMethod ("chmAddProperty",
    signature = c(chm="ngchm", label="character", value="character"),
    definition = function (chm, label, value) {
	chm@properties <- append (chm@properties, new (Class="ngchmProperty", label=label, value=value));
        chm
});

#' @rdname chmAddSpecificAxisTypeFunction-method
#' @aliases chmAddSpecificAxisTypeFunction,ngchm,character,character,character,ngchmJS-method
#'
setMethod ("chmAddSpecificAxisTypeFunction",
    signature = c(chm="ngchm", where="character", type="character", label="character", func="ngchmJS"),
    definition = function (chm, where, type, label, func) {
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
	chm
    }
);

#' @rdname chmAddSpecificAxisTypeFunction-method
#' @aliases chmAddSpecificAxisTypeFunction,ngchm,character,character,character,character-method
#'
setMethod ("chmAddSpecificAxisTypeFunction",
    signature = c(chm="ngchm", where="character", type="character", label="character", func="character"),
    definition = function (chm, where, type, label, func) {
	chmAddSpecificAxisTypeFunction (chm, where, type, label, chmGetFunction (func));
    }
);

#' @rdname chmAddMenuItem-method
#' @aliases chmAddMenuItem,ngchm,character,character,ngchmJS-method
#'
setMethod ("chmAddMenuItem",
    signature = c(chm="ngchm", where="character", label="character", func="ngchmJS"),
    definition = function (chm, where, label, func) {
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
	dup <- 0;
	if (is.list(chm@javascript)) {
	    for (ii in 1:length(chm@javascript))
	        if (chm@javascript[[ii]]@name == func@name) {
		    dup <- ii;
		    if (chm@javascript[[ii]]@script != func@script)
		        stop (sprintf ("chmAddMenuItem: duplicate definition of function '%s' differs from first definition", func@name));
		}
	}
	if (dup == 0)
	    chm@javascript = append (chm@javascript, func);
	chm
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
	at <- new (Class="ngchmAxisType", where=where, type=type, func=func);
	chm@axisTypes <- append (chm@axisTypes, at);
	chm
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

#' @rdname chmAddClassBar-method
#' @aliases chmAddClassBar,ngchm,character,ngchmBar-method
#'
setMethod ("chmAddClassBar",
    signature = c(chm="ngchm", where="character", bar="ngchmBar"),
    definition = function (chm, where, bar) {
	validateNewClassbar (chm, where, bar);
	if (where == "row" || where == "both") {
	    chm@rowClassbars <- append (chm@rowClassbars, bar);
	    if (where == "both")
		chm@colClassbars <- append (chm@colClassbars, bar);
	} else if (where == "column") {
	    chm@colClassbars <- append (chm@colClassbars, bar);
	} else {
	    stop (sprintf ("chmAddClassBar: unknown where '%s'. Should be row, column, or both.", where));
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
	        stop ("binding name does not match corresponding parameter");
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

## @rdname chmRowOrder-method
#' @aliases chmRowOrder<-,ngchm,optDendrogram-method
setReplaceMethod ("chmRowOrder",
    signature = c(chm="ngchm", value="optDendrogram"),
    definition = function (chm, value) {
	if (class(value) == "file") {
	    value <- readLines (value);
	    class(value) <- "fileContent";
	}
	chm@rowOrder <- value
        chm
});

## @rdname chmColOrder-method
#' @aliases chmColOrder<-,ngchm,optDendrogram-method
setReplaceMethod ("chmColOrder",
    signature = c(chm="ngchm", value="optDendrogram"),
    definition = function (chm, value) {
	if (class(value) == "file") {
	    value <- readLines (value);
	    class(value) <- "fileContent";
	}
	chm@colOrder <- value
        chm
});

## @rdname chmRowMeta-method
#' @aliases chmRowMeta<-,ngchm,optList-method
setReplaceMethod ("chmRowMeta",
    signature = c(chm="ngchm", value="optList"),
    definition = function (chm, value) {
	chm@rowMeta <- value
        chm
});

## @rdname chmColMeta-method
#' @aliases chmColMeta<-,ngchm,optList-method
setReplaceMethod ("chmColMeta",
    signature = c(chm="ngchm", value="optList"),
    definition = function (chm, value) {
	chm@colMeta <- value
        chm
});


#' @rdname chmAddToolbox-method
#' @aliases chmAddToolbox,ngchm,character,character,character,character-method
setMethod ("chmAddToolbox",
    signature = c(CHM="ngchm", axis="character", axistype="character", datasetname="character", idstr="character"),
    definition = function (CHM, axis, axistype, datasetname, idstr) {
	toolbox <- ngchm.env$toolbox;
	if (length(toolbox)>0) {
	    for (ii in 1:nrow(toolbox)) {
		if (toolbox[ii,]$type == "GS") {
		    fnname <- sprintf ("%s%s", toolbox[ii,]$fn@name, datasetname);
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

#' @rdname chmAddToolbox2-method
#' @aliases chmAddToolbox2,ngchm,character,character-method
setMethod ("chmAddToolbox2",
    signature = c(CHM="ngchm", datasetname="character", idstr="character"),
    definition = function (CHM, datasetname, idstr) {
	toolbox <- ngchm.env$toolbox;
	if (length(toolbox)>0) {
	    for (ii in 1:nrow(toolbox)) {
		if (toolbox[ii,]$type == "GG") {
		    fnname <- sprintf ("%s%s", toolbox[ii,]$fn@name, datasetname);
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
