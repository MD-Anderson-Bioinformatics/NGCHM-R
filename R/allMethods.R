
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
    if (length(server@deployServer) == 0) {
        stop ("NGCHMs cannot be automatically installed on this server. Please obtain installation instructions from the server administrator.");
    }
    # Install the compiled CHM in outDir/chmName to username@deployServerName:deployDir/chmName
    # Special care is needed to get the permissions correct.
    if (server@deployServer == system("/bin/hostname -f", intern=TRUE)) {
	systemCheck (sprintf ("/bin/mkdir %s/%s", shQuote(chmDeployDir(server)), shQuote(chmName(chm))));
	systemCheck (sprintf ("/bin/chmod g+s %s/%s", shQuote(chmDeployDir(server)), shQuote(chmName(chm))));
	systemCheck (sprintf ("/bin/cp -r %s/%s/* %s/%s/", shQuote(chm@outDir), shQuote(chmName(chm)), shQuote (chmDeployDir(server)), shQuote(chmName(chm))));
	systemCheck (sprintf ("/bin/chmod -R go+rwx %s/%s", shQuote(chmDeployDir(server)), shQuote(chmName(chm))));
    } else {
	dest <- getServerDest (server);
	systemCheck (sprintf ("ssh %s /bin/mkdir %s/%s", dest, shQuote(chmDeployDir(server)), shQuote(chmName(chm))));
	systemCheck (sprintf ("ssh %s /bin/chmod g+s %s/%s", dest, shQuote(chmDeployDir(server)), shQuote(chmName(chm))));
	systemCheck (sprintf ("scp -r %s/%s/* %s:%s/%s/", shQuote(chm@outDir), shQuote(chmName(chm)), dest, shQuote (chmDeployDir(server)), shQuote(chmName(chm))));
	systemCheck (sprintf ("ssh %s /bin/chmod -R go+rwx %s/%s", dest, shQuote(chmDeployDir(server)), shQuote(chmName(chm))));
    }
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
    # Uninstall the installed CHM, if any, at server@serverName:server@deployDir/chm
    # We create the delete.txt special file, wait for the server to do its thing, then remove any leftover trash.
    if (server@deployServer == system("/bin/hostname -f", intern=TRUE)) {
	system (sprintf ("/bin/find %s/%s -type d -exec /bin/chmod g+s '{}' ';'", shQuote(chmDeployDir(server)), shQuote(chm)));
	system (sprintf ("/bin/sh -c \"'(cd %s; touch %s/delete.txt; while [ -O %s/delete.txt ] ; do sleep 1; done; rm -rf %s)'\"",
			 shQuote (chmDeployDir(server)),
			 shQuote (chm),
			 shQuote (chm),
			 shQuote (chm),
			 shQuote (chm)));
    } else {
	system (sprintf ("ssh %s /bin/find %s/%s -type d -exec /bin/chmod g+s '{}' '\\;'", getServerDest (server), shQuote(chmDeployDir(server)), shQuote(chm)));
	system (sprintf ("ssh %s /bin/sh -c \"'(cd %s; touch %s/delete.txt; while [ -O %s/delete.txt ] ; do sleep 1; done; rm -rf %s)'\"",
			 getServerDest (server),
			 shQuote (chmDeployDir(server)),
			 shQuote (chm),
			 shQuote (chm),
			 shQuote (chm),
			 shQuote (chm)));
    }
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
    if (server@deployServer == system("/bin/hostname -f", intern=TRUE)) {
	system (sprintf ("/bin/sh -c \"'(cd %s; touch %s/hidden.txt)'\"",
			 shQuote (chmDeployDir(server)),
			 shQuote (chm)));
    } else {
	system (sprintf ("ssh %s /bin/sh -c \"'(cd %s; touch %s/hidden.txt)'\"",
			 getServerDest (server),
			 shQuote (chmDeployDir(server)),
			 shQuote (chm)));
    }
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
    if (server@deployServer == system("/bin/hostname -f", intern=TRUE)) {
	system (sprintf ("/bin/sh -c \"'(cd %s; /bin/rm %s/hidden.txt)'\"",
			 shQuote (chmDeployDir(server)),
			 shQuote (chm)));
    } else {
	system (sprintf ("ssh %s /bin/sh -c \"'(cd %s; /bin/rm %s/hidden.txt)'\"",
			 getServerDest (server),
			 shQuote (chmDeployDir(server)),
			 shQuote (chm)));
    }
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
               "function _chm_e(sr,ax,fn){function c2(a,b){return a.concat(b);};",
               "return sr.map(function(r){var v=[];for(var ii=r.start;ii<=r.end;ii++)v.push(ii);",
	       "return v.map(function(i){return fn(ax,i);}).reduce(c2);}).reduce(c2);}\n", sep="\n");

writeCustomJS <- function (chm) {
    chan <- file (paste (chm@inpDir, "custom.js", sep="/"), "w");
    if (is.list(chm@javascript)) writeJS (chm@javascript, chan, TRUE);
    cat (startcust, file=chan);
    if (is.list(chm@javascript)) writeJS (chm@javascript, chan, FALSE);
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
	if (js[[ii]]@global == writeGlobals)
	    cat (sprintf ("%s\n", js[[ii]]@script), file=chan);
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
    chm@extrafiles <- c(chm@extrafiles, sprintf ("%s.tsv", dataset@name));
    chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-index.tsv", dataset@name));

    write.table (dataset@data, file.path (dir, sprintf ("%s.tsv", dataset@name)), sep="\t", quote=FALSE);
    systemCheck (sprintf ("/home/bmbroom/my.software/tsvio/tsv_genindex %s %s",
                 shQuote (file.path (dir, sprintf ("%s.tsv", dataset@name))),
                 shQuote (file.path (dir, sprintf ("%s-index.tsv", dataset@name)))));
    if (length(dataset@row.properties) > 0) {
        write.table(dataset@row.properties,
	            file.path (dir, sprintf ("%s-row-properties.tsv", dataset@name)),
		    sep="\t", quote=FALSE, row.names=FALSE);
	chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-row-properties.tsv", dataset@name));
    }
    if (length(dataset@column.properties) > 0) {
        write.table(dataset@column.properties,
	            file.path (dir, sprintf ("%s-column-properties.tsv", dataset@name)),
		    sep="\t", quote=FALSE, row.names=FALSE);
	chm@extrafiles <- c(chm@extrafiles, sprintf ("%s-column-properties.tsv", dataset@name));
    }
    chm
}

writeTemplate <- function (template, outDir) {
    if (length(template@substitutions) == 0) {
        systemCheck (sprintf ("/bin/cp %s %s",
	                       shQuote (template@source.path),
			       shQuote (file.path (outDir, template@dest.path))));
    } else {
        data <- readLines (template@source.path);
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
    rowtypes <- getAllAxisTypes (chm, "row");
    rowTypeFnsReqd <- rep(FALSE,length(rowtypes));
    rowfns <- getAllAxisTypeFunctions (chm, rowtypes$types);
    if (length(rowfns) > 0)
        for (ii in 1:length(rowfns)) {
	    fn <- rowfns[[ii]];
	    rowTypeFnsReqd[getFnsRqrd(rowtypes,fn@type)] <- TRUE;
	    entry <- new (Class="ngchmMenuItem", label=fn@label, description=fn@func@description,
	        fun = sprintf ("function(s,a,e){%s(%s);}", fn@func@name, getValueExpr(rowtypes,fn@type,"axis")));
	    chm@rowMenu <- append (chm@rowMenu, entry);
	}

    # Add column menu items for types we know about.
    coltypes <- getAllAxisTypes (chm, "column");
    colTypeFnsReqd <- rep(FALSE,length(coltypes));
    colfns <- getAllAxisTypeFunctions (chm, coltypes$types);
    if (length(colfns) > 0)
        for (ii in 1:length(colfns)) {
	    fn <- colfns[[ii]];
	    colTypeFnsReqd[getFnsRqrd(coltypes,fn@type)] <- TRUE;
	    entry <- new (Class="ngchmMenuItem", label=fn@label, description=fn@func@description,
	        fun = sprintf ("function(s,a,e){%s(%s);}", fn@func@name, getValueExpr(coltypes,fn@type,"axis")));
	    chm@colMenu <- append (chm@colMenu, entry);
	}

    # Add matrix element menu items for types we know about.
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
    cat ("chmMake: matfns contains ", length(matfns), " entries\n", file=stderr());
    fns <- append (matfns, unique (append (rowfns, colfns)));
    if (length(fns) > 0)
         for (ii in 1:length(fns))
	     chm <- chmAddMenuItem (chm, "nowhere", "unused", fns[[ii]]@func);

    fns <- unique (append (rowtypes$builders[rowTypeFnsReqd], coltypes$builders[colTypeFnsReqd]));
    if (length(fns) > 0)
         for (ii in 1:length(fns))
	     chm <- chmAddMenuItem (chm, "nowhere", "unused", fns[[ii]]@func);

    system (sprintf ("/bin/rm -rf %s", chm@inpDir));
    systemCheck (sprintf ("/bin/mkdir %s", chm@inpDir));

    props = file (chm@propFile, "w");
    cat (sprintf ("# This NGCHM property description was produced using the R NGCHM library version %s at %s\n",
                  packageDescription("NGCHM")$Version, date()), file=props);
    cat (sprintf ("data.set.name=%s\n", chm@name), file=props);
    cat (sprintf ("chm.main.image.height=%d\n", chm@height), file=props);
    cat (sprintf ("chm.main.image.width=%d\n", chm@width), file=props);
    if (length (chm@tags) > 0)
        cat (sprintf ("tags=%s\n", paste(chm@tags,sep=",",collapse=",")), file=props);
    for (ii in 1:length(chm@colormaps))
	writeColorMap ("main", chm@colormaps[[ii]], sprintf("colormap%d", ii), "", props);
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
    chm <- writeChmExtraSupport (chm);
    if (length (chm@extrafiles) > 0)
        cat (sprintf ("additional.input=%s\n", paste(chm@extrafiles,sep="",collapse=",")), file=props);
    close (props);

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

#' @rdname chmMake-method
#' @aliases chmMake,ngchmServer,ngchm-method
#'
setMethod ("chmMake",
    signature = c(server="ngchmServer", chm="ngchm"),
    definition = function (server, chm, deleteOld=TRUE, useJAR=NULL) {
    writeChm (chm);
    if (deleteOld && (length(server@deployServer) > 0)) {
	cat ("chmMake: uninstalling existing NGCHM (if any)\n", file=stderr());
        chmUninstall (server, chm);
	cat ("chmMake: existing NGCHM (if any) has been removed\n", file=stderr());
    }
    system (sprintf ("/bin/mkdir -p %s", shQuote (chm@outDir)));
    system (sprintf ("/bin/rm -rf %s/%s", shQuote (chm@outDir), shQuote(chm@name)));
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
    systemCheck (sprintf ("java -Djava.awt.headless=true -jar %s %s %s %s",
		  shQuote (useJAR),
		  shQuote (chm@inpDir),
		  shQuote (chm@propFile),
		  shQuote (chm@outDir)));
    cat ("chmMake: Java process completed\n", file=stderr());
    writeChmPost (chm);
    systemCheck (sprintf ("/bin/tar czf %s.ngchm.gz -C %s %s",
                          shQuote (chm@name),
			  shQuote (chm@outDir),
			  shQuote (chm@name)));
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

#' Add a file template to the NGCHM.
#'
#' @param chm The chm to add the file template to.
#' @param source.path A string giving the path to the template.
#' @param dest.path A string giving the relative path where to store the template in the generated CHM.
#' @param substitutions A list (may be empty) and substitutions to make in the template.
#'
#' @return The extended chm.
#'
#' @exportMethod chmAddTemplate
#'
#' @rdname chmAddTemplate-method
#' @aliases chmAddTemplate,ngchm,character,character,optList-method
#'
setMethod ("chmAddTemplate",
    signature = c(chm="ngchm", source.path="character", dest.path="character", substitutions="optList"),
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

#' @rdname chmAddMenuItem-method
#' @aliases chmAddMenuItem,ngchm,character,character,ngchmJS-method
#'
setMethod ("chmAddMenuItem",
    signature = c(chm="ngchm", where="character", label="character", func="ngchmJS"),
    definition = function (chm, where, label, func) {
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
