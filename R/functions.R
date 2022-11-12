#' @import methods
#' @import utils
NULL

#' Function to initialize logging
#'
#' @param log_level One of 'TRACE', 'DEBUG', 'INFO', 'SUCCESS'
#' @param log_file Desired path/name of log file
#' @export
#' @importFrom logger log_threshold
#' @importFrom logger log_layout
#' @importFrom logger log_debug
#' @importFrom logger log_error
#' @importFrom logger log_appender
#' @importFrom logger appender_file
#' @importFrom logger appender_console
#' @importFrom logger layout_glue_generator
#' @importFrom logger layout_glue
initLogging <- function(log_level,log_file=NULL) {
	log_threshold(log_level)
	if (!is.null(log_file)) {
		log_appender(appender_file(log_file))
		log_layout(layout_glue_generator(format = paste(
			'{level}',
			'[{format(time, "%Y-%m-%d %H:%M:%S")}]',
			'{fn}',
			'{msg}')))
		log_info('Writing log to file: ',log_file)
	} else { 
		log_appender(appender_console)
		log_layout(layout_glue_generator(format = paste(
			'{crayon::bold(colorize_by_log_level(level, levelr))}',
			'[{crayon::italic(format(time, "%Y-%m-%d %H:%M:%S"))}]',
			'{crayon::blue(fn)}',
			'{grayscale_by_log_level(msg, levelr)}')))
	}
	log_debug('Set log level to ',log_level)
}

systemCheck <- function (command, ...) {
    # Execute the specified command and halt execution with an error
    # message if it fails.
    status <- system (command, ...)
    if (status != 0) {
        stop ('Error encountered executing system command: ', command)
    }
}

# Returns TRUE if object has any of the specified classes.
#
isany <- function (object, classes) {
    any (vapply (classes, function(cls) is(object, cls), TRUE))
};

# Returns the class(es) of object as a single string.
#
classstr <- function (object) {
    paste (class (object), collapse=",")
};

#' Register an ngchmServer.
#'
#' This function registers an ngchmServer that can be used when
#' making and installing a Next Generation Clustered Heat Map.
#'
#' @param uuid A string that identifies the server namespace.
#' @param server The ngchmServer to register.
#'
#' @return the server that was registered
#' @export
#'
#' @seealso [chmInstall()]
#' @seealso [chmUninstall()]
#' @seealso [ngchmUnregisterServer()]
#' @seealso [ngchmServer-class]
#'
ngchmRegisterServer <- function (uuid, server) {
    obj <- list (uuid=uuid, name=server@name, server=server);
    matches <- which (vapply (ngchm.env$servers, function(srv) ((srv$uuid == uuid) && (srv$name == server@name)), TRUE));
    if (length (matches) > 0) {
	ngchm.env$servers[[matches]] <- obj;
    } else {
	ngchm.env$servers <- append (ngchm.env$servers, list(obj));
    }
    server
}

#' Unregister ngchmServer(s).
#'
#' This function unregisters one or more ngchmServer(s).
#'
#' @param uuid A string that identifies the server namespace.
#' @param name The names(s) of the ngchmServer(s) to unregister.  If not specified, all ngchmServers in the
#' namespace are unregistered.
#'
#' @export
#'
#' @seealso [ngchmRegisterServer()]
#' @seealso [ngchmServer-class]
#'
ngchmUnregisterServer <- function (uuid, name=NULL) {
    if (length(name) == 0) {
	matches <- vapply (ngchm.env$servers, function(srv) (srv$uuid == uuid), TRUE);
    } else {
        matches <- vapply (ngchm.env$servers, function(srv) ((srv$uuid == uuid) && (srv$name %in% name)), TRUE);
    }
    if (sum (matches) > 0) {
	ngchm.env$servers <- ngchm.env$servers[!matches];
    }
}

#' Get a registered ngchmServer object for use in making and installing NGCHMs
#'
#' This function returns a ngchmServer object that can be used when
#' making and installing a Next Generation Clustered Heat Map.
#'
#' @param name The name of the ngchmServer desired.
#'
#' @return An object of class ngchmServer if found, NULL otherwise.  If multiple servers of the same
#' name have been defined (in different namespaces), the most recently defined is returned.
#'
#' @export
#'
#' @seealso [chmInstall()]
#' @seealso [chmUninstall()]
#' @seealso [ngchmServer-class]
#'
chmServer <- function (name) {
    matches <- which (vapply (ngchm.env$servers, function(srv) (srv$name == name), TRUE));
    if (length (matches) > 0) {
	return (ngchm.env$servers[[matches[length(matches)]]]$server);
    } else {
	return (NULL);
    }
}

#' Get the ngchm environment (for debugging only).
#'
#' Get the library's internal ngchm environment to help debugging.
#'
#' @export
ngchmGetEnv <- function () {
    return (ngchm.env);
}

#' Create a new NGCHM.
#'
#' This function creates a Next Generation Clustered Heat Map (NGCHM) object in memory.
#' Additional parameters will be added to the new NGCHM (see chmAdd).
#' The bare NGCHM needs at least one data layer added to it before it can be compiled.
#'
#' @param name The name under which the NGCHM will be saved to the NGCHM server.
#' @param ... Zero or more initial objects to include in the NGCHM (see chmAdd).
#' @param rowOrder A vector, dendrogram, or function specifying the CHM row order.
#' @param rowDist Distance method to use by default RowOrder
#' @param rowAgglom Agglomeration method to use by default RowOrder
#' @param colOrder A vector, dendrogram, or function specifying the CHM column order.
#' @param colDist Distance method to use by default ColOrder
#' @param colAgglom Agglomeration method to use by default ColOrder
#' @param rowAxisType The type(s) of the row labels (default: None).
#' @param colAxisType The type(s) of the column labels (default: None).
#' @param rowCovariates Covariate(Bar)(s) to add to the rows (default: None).
#' @param colCovariates Covariate(Bar)(s) to add to the columns (default: None).
#' @param format The format of NGCHM to produce (default: 'original').
#' @param rowGapLocations Locations for row gaps. Specify as a list of integers or [chmTreeGaps()] function.
#' @param colGapLocations Locations for col gaps. Specify as a list of integers or [chmTreeGaps()] function.
#' @param rowGapWidth Width of row gaps (default: 5 rows)
#' @param colGapWidth Width of col gaps (default: 5 cols)
#' @param overview The format(s) of overview image(s) to create (default: None).
#' @param logLevel The level of logs to output
#' @param logFile The file to which logs should be output
#' @importFrom logger log_debug
#' @importFrom logger log_error
#'
#' @return An object of class ngchm
#'
#' @export
#'
#' @examples
#' mychm <- chmNew ("test_chm")
#' mychm <- chmNew("test_chm", rowGapLocations=c(3,5))
#' mychm <- chmNew("test_chm", rowGapLocations=chmTreeGaps(4))
#' mychm <- chmNew("test_chm", rowGapWidth=3)
#'
#' @seealso [ngchm-class]
#' @seealso [ngchmServer-class]
#' @seealso [chmAdd()]
#' @seealso [chmAddAxisType()]
#' @seealso [chmAddCovariateBar()]
#' @seealso [chmAddProperty()]
#' @seealso [chmAddOverview()]
#' @seealso [chmInstall()]
#' @seealso [chmExportToFile()]
#' @seealso [chmExportToPDF()]
#' @seealso [chmExportToHTML()]

chmNew <- function (name, ...,
                    rowOrder=chmDefaultRowOrder, rowDist="correlation", rowAgglom="ward.D2",
		    colOrder=chmDefaultColOrder, colDist="correlation", colAgglom="ward.D2",
                    rowAxisType=NULL, colAxisType=NULL,
		    rowCovariates=NULL, colCovariates=NULL,
                    format="original",
		rowGapLocations=NULL,
		rowGapWidth=5,
		colGapLocations=NULL,
		colGapWidth=5,
		    overview=c(),
		logLevel='INFO', logFile=NULL) {
	initLogging(logLevel, logFile)

    chm <- new (Class="ngchmVersion2",
                name=name,
                format=format,
		inpDir=utempfile("ngchm.input"),
		outDir=utempfile("ngchm.output"),
                rowDist=rowDist,
                rowAgglom=rowAgglom,
                colDist=colDist,
                colAgglom=colAgglom,
                rowOrderMethod="",
                colOrderMethod="",
		rowCutLocations=rowGapLocations,
		rowCutWidth=rowGapWidth,
		colCutLocations=colGapLocations,
		colCutWidth=colGapWidth
	);
    chmRowOrder(chm) <- rowOrder;
    chmColOrder(chm) <- colOrder;
    chm@uuid <- getuuid (name);
    chm <- chmAddCSS (chm, 'div.overlay { border: 2px solid yellow; }');

    chm <- chmAddList (chm, list(...));
    if (!is.null(rowAxisType)) chm <- chmAddAxisType (chm, 'row', rowAxisType);
    if (!is.null(colAxisType)) chm <- chmAddAxisType (chm, 'column', colAxisType);
    if (!is.null(rowCovariates)) {
        if (is.list (rowCovariates)) {
	    for (cov in rowCovariates) chm <- chmAddCovariateBar (chm, 'row', cov);
	} else {
	    chm <- chmAddCovariateBar (chm, 'row', cov);
	}
    }
    if (!is.null(colCovariates)) {
        if (is.list (colCovariates)) {
	    for (cov in colCovariates) chm <- chmAddCovariateBar (chm, 'column', cov);
	} else {
	    chm <- chmAddCovariateBar (chm, 'col', cov);
	}
    }
    for (ov in overview) chm <- chmAddOverview (chm, ov, NULL, NULL);
    chm
}

#' Compute cosine (angular) distance matrix
#'
#' @param data A numeric matrix
cos.dist1 <- function (data) {
    dmat <- matrix (0.0, nrow=nrow(data), ncol=nrow(data))
    sumrowsqr <- sqrt (apply (data*data, 1, sum))
    inner <- data %*% t(data)
    dmat <- inner / (sumrowsqr %*% t(sumrowsqr))
    # Adjust distance matrix to avoid clustering error with identical rows.
    rrr <- matrix (abs(rnorm(length(dmat), sd=1e-10)), nrow=nrow(dmat));
    dmat <- (1-rrr) * dmat;
    return (2/pi*acos(as.dist(dmat)));
}

#' Return default column order of an NGCHM
#'
#' @param chm An NGCHM containing at least one layer
#'
#' @return Shaid of a dendrogram suitable for use as the
#'         chm's column order.
#'
#' @export
chmDefaultColOrder <- function (chm) {
    chm <- chmFixVersion (chm);
    if (length (chm@layers) == 0) stop ("chm requires at least one layer");
    shaidyRepo <- ngchm.env$tmpShaidy;
    shaid <- chm@layers[[1]]@data;

    provid <- shaidyProvenance (shaidyRepo, name="chmDefaultColOrder",
                                shaid=shaid@value, dist=chm@colDist,
                                agglom=chm@colAgglom);
    res <- shaidyRepo$provenanceDB$get ('dendrogram', provid);
    if (length(res) == 0) {
        mat <- ngchmLoadDatasetBlob (shaidyRepo, shaid)$mat;
	if (chm@colDist == "correlation") {
	    dd <- as.dist(1-cor(mat, use="pairwise"));
	} else if (chm@colDist == "cosine") {
            dd <- cos.dist1 (t(mat));
	} else {
	    dd <- dist (t(mat), method=chm@colDist);
	}
	ddg <- stats::as.dendrogram(stats::hclust(dd, method=chm@colAgglom))
	res <- list(ngchmSaveAsDendrogramBlob (shaidyRepo, ddg));
	shaidyRepo$provenanceDB$insert (provid, res[[1]]);
    }
    res[[1]]
}

#' Convert a user specified dendrogram to a shaid
#'
#' @param ddg The dendrogram to convert.
chmUserDendrogramToShaid <- function (ddg) {
    shaidyRepo <- ngchm.env$tmpShaidy;
    ngchmSaveAsDendrogramBlob (shaidyRepo, ddg)
}

#' Convert user specified labels to a shaid
#'
#' @param labels A string vector containing axis labels.
chmUserLabelsToShaid <- function (labels) {
    shaidyRepo <- ngchm.env$tmpShaidy;
    ngchmSaveLabelsAsBlob (shaidyRepo, labels)
}

#' Return default row order of an NGCHM
#'
#' @param chm An NGCHM containing at least one layer
#'
#' @return Shaid of a dendrogram suitable for use as the
#'         chm's row order.
#'
#' @export
chmDefaultRowOrder <- function (chm) {
    chm <- chmFixVersion (chm);
    if (length (chm@layers) == 0) stop ("chm requires at least one layer");
    shaidyRepo <- ngchm.env$tmpShaidy;
    shaid <- chm@layers[[1]]@data;

    provid <- shaidyProvenance (shaidyRepo, name="chmDefaultRowOrder",
                                shaid=shaid@value, dist=chm@rowDist,
                                agglom=chm@rowAgglom);
    res <- shaidyRepo$provenanceDB$get ('dendrogram',provid);
    if (length(res) == 0) {
        mat <- ngchmLoadDatasetBlob (shaidyRepo, shaid)$mat;
	if (chm@rowDist == "correlation") {
	    dd <- as.dist(1-cor(t(mat), use="pairwise"))
	} else if (chm@rowDist == "cosine") {
            dd <- cos.dist1 (mat);
	} else {
	    dd <- dist (mat, method=chm@rowDist);
	}
	ddg <- stats::as.dendrogram(stats::hclust(dd, method=chm@rowAgglom));
	res <- list(ngchmSaveAsDendrogramBlob (shaidyRepo, ddg));
	shaidyRepo$provenanceDB$insert (provid, res[[1]]);
    }
    res[[1]]
}

#' Return original row order of an NGCHM
#'
#' @param chm An NGCHM containing at least one layer
#'
#' @return Shaid of a label order suitable for use as the
#'         chm's row order.
#'
#' @export
chmOriginalRowOrder <- function (chm) {
    chm <- chmFixVersion (chm);
    if (length (chm@layers) == 0) stop ("chm requires at least one layer");
    shaidyRepo <- ngchm.env$tmpShaidy;
    shaid <- chm@layers[[1]]@data;

    provid <- shaidyProvenance (shaidyRepo, name="chmOriginalRowOrder",
                                shaid=shaid@value);
    res <- shaidyRepo$provenanceDB$get ('label',provid);
    if (length(res) == 0) {
        mat <- ngchmLoadDatasetBlob (shaidyRepo, shaid)$mat;
        labels <- rownames(mat);
        labels.shaid <- ngchmSaveLabelsAsBlob (shaidyRepo, labels);
	shaidyRepo$provenanceDB$insert (provid, labels.shaid);
        res <- list(labels.shaid);
    }
    res[[1]]
}

#' Return random row order of an NGCHM
#'
#' @param chm An NGCHM containing at least one layer
#'
#' @return Shaid of a label order suitable for use as the
#'         chm's row order.
#'
#' @export
chmRandomRowOrder <- function (chm) {
    chm <- chmFixVersion (chm);
    if (length (chm@layers) == 0) stop ("chm requires at least one layer");
    shaidyRepo <- ngchm.env$tmpShaidy;
    shaid <- chm@layers[[1]]@data;

    provid <- shaidyProvenance (shaidyRepo, name="chmRandomRowOrder",
                                shaid=shaid@value);
    res <- shaidyRepo$provenanceDB$get ('label',provid);
    if (length(res) == 0) {
        mat <- ngchmLoadDatasetBlob (shaidyRepo, shaid)$mat;
        labels <- rownames(mat)[sample.int(nrow(mat),nrow(mat))];
        labels.shaid <- ngchmSaveLabelsAsBlob (shaidyRepo, labels);
	shaidyRepo$provenanceDB$insert (provid, labels.shaid);
        res <- list(labels.shaid);
    }
    res[[1]]
}

#' Return original column order of an NGCHM
#'
#' @param chm An NGCHM containing at least one layer
#'
#' @return Shaid of a label order suitable for use as the
#'         chm's column order.
#'
#' @export
chmOriginalColOrder <- function (chm) {
    chm <- chmFixVersion (chm);
    if (length (chm@layers) == 0) stop ("chm requires at least one layer");
    shaidyRepo <- ngchm.env$tmpShaidy;
    shaid <- chm@layers[[1]]@data;

    provid <- shaidyProvenance (shaidyRepo, name="chmOriginalColOrder",
                                shaid=shaid@value);
    res <- shaidyRepo$provenanceDB$get ('label',provid);
    if (length(res) == 0) {
        mat <- ngchmLoadDatasetBlob (shaidyRepo, shaid)$mat;
        labels <- colnames(mat);
        labels.shaid <- ngchmSaveLabelsAsBlob (shaidyRepo, labels);
	shaidyRepo$provenanceDB$insert (provid, labels.shaid);
        res <- list(labels.shaid);
    }
    res[[1]]
}

#' Return random column order of an NGCHM
#'
#' @param chm An NGCHM containing at least one layer
#'
#' @return Shaid of a label order suitable for use as the
#'         chm's column order.
#'
#' @export
chmRandomColOrder <- function (chm) {
    chm <- chmFixVersion (chm);
    if (length (chm@layers) == 0) stop ("chm requires at least one layer");
    shaidyRepo <- ngchm.env$tmpShaidy;
    shaid <- chm@layers[[1]]@data;

    provid <- shaidyProvenance (shaidyRepo, name="chmRandomColOrder",
                                shaid=shaid@value);
    res <- shaidyRepo$provenanceDB$get ('label',provid);
    if (length(res) == 0) {
        mat <- ngchmLoadDatasetBlob (shaidyRepo, shaid)$mat;
        labels <- colnames(mat)[sample.int(ncol(mat),ncol(mat))];
        labels.shaid <- ngchmSaveLabelsAsBlob (shaidyRepo, labels);
	shaidyRepo$provenanceDB$insert (provid, labels.shaid);
        res <- list(labels.shaid);
    }
    res[[1]]
}

chmAddAxis <- function (chm, axis) {
    stopifnot (is(chm, "ngchm") && is(axis,"ngchmAxis"));
    where <- match.arg (axis@axis, c("row", "column", "both"));
    chm <- chmFixVersion (chm);
    for (item in axis@objects) {
	if (is(item, "ngchmAxisType")) {
            item@where <- where;
	    chm@axisTypes <- append (chm@axisTypes, item);
	    chm <- chmAddProperty (chm, paste('!axistype', where, sep='.'), item@type)
        } else if (isany (item, c("ngchmBar", "ngchmCovariate"))) {
	    chm <- chmAddCovariateBar (chm, where, item);
	} else {
	    stop (sprintf ("Unable to add item of class '%s' to ngchm axis\n", classstr(item)));
	}
    }
    chm
}

# Function used by chmNew and chmAdd:
chmAddList <- function (chm, args) {
    #cat (sprintf ("chmAdd: %d items to add\n", length(args)), file=stderr());
    for (item in args) {
	if (is(item, "ngchmLayer")) { chm <- chmAddLayer (chm, item); }
	else if (is(item, "matrix") && (mode(item) == "numeric")) { chm <- chmAddLayer (chm, item); }
	else if (is(item, "ngchmDataset")) { chm <- chmAddDataset (chm, item); }
	else if (is(item, "ngchmColormap")) { chm <- chmAddColormap (chm, item); }
	else if (is(item, "ngchmDialog")) { chm <- chmAddDialog (chm, item); }
	else if (is(item, "ngchmProperty")) { chmProperty(chm, item@label) <- item; }
	else if (is(item, "ngchmAxis")) { chm <- chmAddAxis (chm, item); }
        else if (is(item, "list")) { chm <- chmAddList (chm, item); }
	else {
	    stop (sprintf ("Unable to add item of class '%s' to ngchm\n", classstr(item)));
	}
    }
    chm
}

#' Create a new Data Layer for a NGCHM.
#'
#' This function creates a new Data Layer suitable for adding to a Next Generation Clustered Heat Map.
#'
#' @param label The name under which the data layer will be displayed to the user.
#' @param data A matrix containing the data to display. Must have rownames and colnames.
#' @param colors A color map specifying how the data should be rendered.  If omitted or NULL,
#' a default green-black-red color map will be estimated from the data.
#' @param summarizationMethod The method to use when summarizing multiple data points per pixel.  Possible values are average (default), sample, and mode.
#' @param cuts_color color of cuts 
#'
#' @return An object of class ngchmLayer
#'
#' @export
#'
#' @examples
#' noise <- matrix (runif(1000) + runif(1000*1000), nrow=1000)
#' rownames(noise) <- sprintf ("Row%d", 1:nrow(noise))
#' colnames(noise) <- sprintf ("Col%d", 1:ncol(noise))
#' noise.colors <- chmNewColorMap (c(0,1,2),
#'                                 c("green", "black", "red"), missing.color="yellow")
#' layer <- chmNewDataLayer ("Noisy Data", noise, noise.colors)
#'
#' @seealso [ngchmLayer-class]
#' @seealso [chmNewColorMap()]
#' @seealso [chmAddLayer()]
#'
chmNewDataLayer <- function (label, data, colors, summarizationMethod, cuts_color) {
    if (typeof (label) != "character") {
        stop (sprintf ("Parameter 'label' must have type 'character', not '%s'", typeof(label)));
    }
    if (length (label) != 1) {
        stop (sprintf ("Parameter 'label' must have a single value, not %d", length(label)));
    }
    if (nchar (label) == 0) {
        stop ("Parameter 'label' cannot be the empty string");
    }
    if (missing(colors)) colors <- NULL;
    if (missing(summarizationMethod)) summarizationMethod <- "average";
    if (missing(cuts_color)) cuts_color <- "#4c4c4c";
    if (typeof (summarizationMethod) != "character") {
        stop (sprintf ("Parameter 'summarizationMethod' must have type 'character', not '%s'", typeof(summarizationMethod)));
    }
    if (length (summarizationMethod) != 1) {
        stop (sprintf ("Parameter 'summarizationMethod' must have a single value, not %d", length(summarizationMethod)));
    }
    if (typeof (cuts_color) != "character") {
        stop (sprintf ("Parameter 'cuts_color' must have type 'character', not '%s'", typeof(cuts_color)));
    }
    if (length (cuts_color) != 1) {
        stop (sprintf ("Parameter 'cuts_color' must have a single value, not %d", length(cuts_color)));
    }
    summarizationMethod = match.arg (summarizationMethod, c("average", "sample", "mode"));
    data <- ngchmSaveAsDatasetBlob (ngchm.env$tmpShaidy, 'tsv', data);
    if (length(colors) == 0)
	colors <- chmNewColorMap (data, c("#0010a0", "#f0f0f0", "#a01000"), missing='#ff00ff'); # Blue, Off-white, Red. Missing=bright magenta.
    new (Class="ngchmLayer", name=label, data=data, colors=colors, summarizationMethod=summarizationMethod, cuts_color=cuts_color)
};

#' Get a specified Data Layer from an NG-CHM.
#'
#' This function returns a Data Layer contained in a Next Generation Clustered Heat Map.
#'
#' @param hm The NG-CHM object to get the data layer from.
#' @param label The name or index of the data layer to get.  If a name, return the layer with
#'        that name.  If no layer with that name exists or if the index is out of range,
#'        return NULL.
#'
#' @return An object of class ngchmLayer or NULL.
#'
#' @export
#'
#' @examples
#' layer <- chmLayer (hm, "Layer 1")
#' layer <- chmLayer (hm, 1)
#'
#' @seealso [ngchmLayer-class]
#'
chmLayer <- function (hm, label) {
    stopifnot (!missing(hm), is(hm, "ngchm"));
    stopifnot (missing(label) || length(label) == 1);
    if (length(hm@layers) == 0) return (NULL);
    # Determine numeric layeridx from specified label
    if (missing (label)) {
    	layeridx <- 1;
    } else if (mode(label) == "character") {
        layernames <- vapply (hm@layers, function(x)x@name, "");
	layeridx <- which(label == layernames);
	if (length(layeridx) == 0) return (NULL);
	if (length(layeridx) > 1) stop ("NGCHM contains multiple layers with that name");
    } else if (mode(label) == "numeric") {
        if ((label < 1) || (label > length(hm@layers))) return (NULL);
	layeridx <- label;
    } else {
        stop ("mode of label must be character or numeric");
    }
    hm@layers[[layeridx]]
};

#' Set (or append) a specified Data Layer in an NG-CHM.
#'
#' This function sets a Data Layer in a Next Generation Clustered Heat Map.
#'
#' @param x The NG-CHM object to set the data layer of
#' @param label The name or index of the data layer to set.  If a name, replace the layer with
#'        that name.  Append a new layer if no layer with that name exists.  If an index,
#'	  replace the specified layer.  If zero (0), prepend the new layer.  If minus one (-1)
#'        or N+1 (for an NG-CHM with N layers), appends a new layer.
#' @param colors A colormap for the new layer.  If missing, defaults to the color map of the
#'        layer being replaced, or to the default new layer color map for a new layer.
#' @param summarizationMethod The summarization method for the new layer.  If
#'        missing, defaults to the summarization method of the layer being
#'        replaced, or to the default new layer summarization method for a new layer.
#' @param cuts_color The cuts color for the new layer.  If
#'        missing, defaults to the cuts color of the layer being
#'        replaced, or to the default cuts color for a new layer.
#' @param value Either a matrix or a data layer to set in the NG-CHM.  If value is a matrix,
#'        the other data layer parameters (label, colors, summarizationMethod,
#'        and cuts_color) are set from the parameters if specified, from the old
#'        data layer (if any), or the defaults for a new data layer (see chmNewDataLayer).
#'        If value is a data layer, any other data layer parameters specified will
#'        override those in the replacement layer.
#'
#' @return An object of class ngchm.
#'
#' @name chmLayer<-
#' @export
#'
#' @examples
#' chmLayer (hm, "Layer 1") <- matrix;
#' chmLayer (hm, 1, cuts_color = "#fefefe") <- chmNewDataLayer ("New data layer", matrix);
#'
#' @seealso [ngchmLayer-class]
#' @seealso chmNewDataLayer
#'
assign('chmLayer<-', function (x, label, colors, summarizationMethod, cuts_color, value) {
    stopifnot (!missing(x), is(x, "ngchm"));
    stopifnot (missing(label) || length(label) == 1);
    stopifnot (!missing(value), isany(value, c("matrix", "ngchmLayer")));
    hm <- chmFixVersion (x);
    # Convert label into a numeric layeridx and a character label
    if (missing(label)) {
	layeridx <- length(hm@layers)+1;
	label <- sprintf ("Layer %d", layeridx);
    } else if (mode(label) == "character") {
        layernames <- vapply (hm@layers, function(x)x@name, "");
	layeridx <- which(label == layernames);
	if (length(layeridx) > 1) stop ("NGCHM contains multiple layers with that name");
	if (length(layeridx) == 0) layeridx <- length(hm@layers) + 1;
    } else if (mode(label) == "numeric") {
        if ((label < -1) || (label > length(hm@layers)+1)) stop("Invalid label");
	if (label < 0) {
	    layeridx <- length(hm@layers) + 1;
	} else {
	    layeridx <- label;
	}
	if (label == 0) {
	    label <- "Layer 0";
	} else if (label > length(hm@layers)) {
	    label <- sprintf ("Layer %d", length(hm@layers)+1);
	} else {
	    label <- hm@layers[[layeridx]]@name;
	}
    } else {
	stop ("mode of label must be either character or numeric")
    }
    if (is (value, "matrix")) {
	if (layeridx <= length(hm@layers)) {
	    tmp <- hm@layers[[layeridx]];
	    if (missing(colors)) colors <- tmp@colors;
	    if (missing(summarizationMethod)) summarizationMethod <- tmp@summarizationMethod;
	    if (missing(cuts_color)) cuts_color <- tmp@cuts_color;
	}
	newlayer <- chmNewDataLayer (label, data=value, colors=colors, summarizationMethod=summarizationMethod, cuts_color=cuts_color);
    } else if (is (value, "ngchmLayer")) {
	newlayer <- value;
	if (!missing(colors)) newlayer@colors <- colors;
	if (!missing(summarizationMethod)) newlayer@summarizationMethod <- summarizationMethod;
	if (!missing(cuts_color)) newlayer@cuts_color <- cuts_color;
    }
    validateNewLayer (hm, newlayer);
    if (layeridx == 0) {
	hm@layers <- c (newlayer, hm@layers);
    } else {
	hm@layers[[layeridx]] <- newlayer;
    }
    chmAddColormap (hm, newlayer@colors)
});

#' Create a new Dataset for a NGCHM.
#'
#' This function creates a new Dataset suitable for attaching to a Next Generation Clustered Heat Map.
#'
#' @param name The filename prefix under which the dataset will be saved to the ngchm.
#' @param description A description of the dataset.
#' @param data A matrix containing the data in the dataset. Must have rownames and colnames.
#' @param row.covariates An optional list of row covariates.
#' @param column.covariates An optional list of column covariates.
#' @param row.type The type, if any, of the dataset rows.
#' @param column.type The type, if any, of the dataset columns.
#'
#' @return An object of class ngchmDataset
#'
#' @export
#'
#' @seealso [ngchmDataset-class]
#' @seealso [ngchmCovariate-class]
#' @seealso [chmAddDataset()]
#'
chmNewDataset <- function (name, description, data,
                           row.type = NULL,
                           column.type = NULL,
                           row.covariates = NULL,
			   column.covariates = NULL) {
    if (typeof (name) != "character") {
        stop (sprintf ("Parameter 'name' must have type 'character', not '%s'", typeof(name)));
    }
    if (length (name) != 1) {
        stop (sprintf ("Parameter 'name' must have a single value, not %d", length(name)));
    }
    if (nchar (name) == 0) {
        stop ("Parameter 'name' cannot be the empty string");
    }
    if (typeof (description) != "character") {
        stop (sprintf ("Parameter 'description' must have type 'character', not '%s'", typeof(description)));
    }
    if (length (description) != 1) {
        stop (sprintf ("Parameter 'description' must have a single value, not %d", length(description)));
    }
    if (nchar (description) == 0) {
        warning ("Parameter 'description' should not be the empty string");
    }
    data <- ngchmSaveAsDatasetBlob (ngchm.env$tmpShaidy, 'tsv', data);
    if (length (row.covariates) > 0) {
        if (is(row.covariates, "ngchmCovariate")) {
	    row.covariates <- list (row.covariates);
	} else if (is(row.covariates, "list")) {
	    stopifnot (all (vapply (row.covariates, function(cov)is(cov,"ngchmCovariate"), TRUE)));
	} else {
	    stop (sprintf ("Parameter 'row.covariates' for dataset '%s' must be either a covariate or list of covariates, not a '%s'",
	                   name, classstr(row.covariates)));
	}
    }
    if (length (column.covariates) > 0) {
        if (is(column.covariates, "ngchmCovariate")) {
	    column.covariates <- list (column.covariates);
	} else if (is(column.covariates, "list")) {
	    stopifnot (all (vapply (column.covariates, function(cov)is(cov,"ngchmCovariate"), TRUE)));
	} else {
	    stop (sprintf ("Parameter 'column.covariates' for dataset '%s' must be either a covariate or list of covariates, not a '%s'",
	                   name, classstr(column.covariates)));
	}
    }
    new (Class="ngchmDataset", name=name, description=description, data=data,
         row.type = row.type,
         column.type = column.type,
         row.covariates = row.covariates,
	 column.covariates=column.covariates);
}

#' Create a new Covariate for adding to an NGCHM auxilary dataset.
#'
#' This function creates a new Covariate suitable for a covariate bar or attaching to an NGCHM auxilary dataset.
#'
#' @param fullname The full (human readable) name of the covariate.
#' @param values A named vector of values (character, logical, or numeric).
#' @param value.properties An ngchmColormap mapping values to properties.
#' @param type The string "discrete" or the string "continuous".  (Defaults to continuous for numeric values, to discrete otherwise.)
#' @param covabbv The short R-compatible identifier used to identify the covariate (derived from fullname if not specified).
#'
#' @return An object of class ngchmCovariate.
#'
#' @export
#'
#' @seealso [ngchmCovariate-class]
#' @seealso [chmAddCovariate()]
#' @seealso [chmNewColorMap()]
#'
chmNewCovariate <- function (fullname, values, value.properties=NULL, type=NULL, covabbv=NULL)
{
    # Validate basic properties of 'fullname'.
    if (typeof (fullname) != "character") {
        stop (sprintf ("Parameter 'fullname' must have type 'character', not '%s'", typeof(fullname)));
    }
    if (length (fullname) != 1) {
        stop (sprintf ("Parameter 'fullname' must have a single value, not %d", length(fullname)));
    }
    if (nchar (fullname) == 0) {
        stop (sprintf ("Parameter 'fullname' cannot be the empty string"));
    }
    # Validate basic properties of 'type'.
    if (length(type) == 0) {
        if (mode(values) == "numeric") {
	    type <- "continuous";
	} else {
	    type <- "discrete";
	}
    } else {
	if (typeof (type) != "character") {
	    stop (sprintf ("Parameter 'type' for covariate '%s' must have type 'character', not '%s'", fullname, typeof(type)));
	}
	if (length (type) != 1) {
	    stop (sprintf ("Parameter 'type' for covariate '%s' must have a single value, not %d", fullname, length(type)));
	}
	if (!(type %in% c("discrete", "continuous"))) {
	    stop (sprintf ("Parameter 'type' for covariate '%s' must be either 'discrete' or 'continuous', not '%s'", fullname, type));
	}
    }

    if ((length (value.properties) > 0) && (!is(value.properties, "ngchmColormap"))) {
	stop (sprintf ("value.properties for covariate '%s' must have class ngchmColormap, not '%s'",
	               fullname, classstr(value.properties)));
    }

    if (type == "continuous") {
        if (mode(values) != "numeric") {
	    stop (sprintf ("Covariate '%s' has type continuous, so values must have mode numeric, not '%s'", fullname, mode(values)));
	}
	if (length (value.properties) == 0) {
	    value.properties <- chmNewColorMap (as.matrix(values,ncol=1));
	}
    } else if (length (value.properties) == 0) {
        if (mode(values) == "numeric") {
	    value.properties <- chmNewColorMap (sort (unique (values)));
	} else if (mode(values) == "logical") {
	    value.properties <- chmNewColorMap (c (FALSE, TRUE));
	} else {
	    # We start with human-readable values
	    # We convert these into machine-readable codes
	    unames <- sort (unique (values));
	    uvals <- make.names(unames);
	    # The properties map uses codes, but maps to the original values.
	    value.properties <- chmNewColorMap (uvals, names=unames);
	    # Replace values with the corresponding codes.
	    values <- values[!is.na(values)];
	    idx <- vapply(values,function(vv)which(unames==vv),1);
	    values[] <- uvals[idx];
	}
    }

    # Validate type of 'values'
    if (mode (values) %in% c("numeric","logical"))
        mode(values) <- "character";

    if (length (covabbv) == 0) {
        covabbv <- make.names(fullname);
    } else {
	if (typeof (covabbv) != "character") {
	    stop (sprintf ("Parameter 'covabbv' must have type 'character', not '%s'", typeof(covabbv)));
	}
	if (length (covabbv) != 1) {
	    stop (sprintf ("Parameter 'covabbv' must have a single value, not %d", length(covabbv)));
	}
	if (nchar (covabbv) == 0) {
	    stop ("Parameter 'covabbv' cannot be the empty string");
	}
    }
    values <- values[order(names(values))];
    mat <- matrix (values, ncol=1, dimnames=list(names(values),'Value'));
    shaid <- ngchmSaveAsDatasetBlob (ngchm.env$tmpShaidy, 'tsv', mat);
    new (Class="ngchmCovariate", label=covabbv, fullname=fullname, type=type,
	 label.series = shaid, series.properties=value.properties);
};

#' Get a covariate attached to an NG-CHM dataset.
#'
#' @param dataset The NG-CHM dataset to get the covariate from.
#' @param fullname The full name of the covariate to get.
#'        If no covariate with that name exists, return NULL.
#' @param where The axis or axes on which to look for the covariate  Can be "row", "column", or "both" (default).
#'
#' @return A ngchmCovariate or NULL.
#'
#' @export
#'
#' @examples
#' chmCovariate (dataset, "Age")
#'
#' @seealso [ngchmCovariate-class]
#' @seealso chmNewCovariate
#' @seealso chmCovariateBar
#'
chmCovariate <- function (dataset, fullname, where) {
    stopifnot (is (dataset, "ngchmDataset"));
    checkLabel (fullname, "fullname");
    where <- if (missing(where)) "both" else match.arg (where, c("row", "column", "both"));
    if (where %in% c("row", "both")) {
	names <- lapply (dataset@row.covariates, function(cv) cv@fullname);
	idx <- which (names == fullname);
	if (length(idx) > 0) {
	    stopifnot (length(idx) == 1);
	    return (dataset@row.covariates[[idx]]);
	}
	if (where == "row") return(NULL);
    }
    # where must be either "column" or "both".
    names <- lapply (dataset@column.covariates, function(cv) cv@fullname);
    idx <- which (names == fullname);
    if (length(idx) > 0) {
	stopifnot (length(idx) == 1);
	return (dataset@column.covariates[[idx]]);
    }
    return(NULL);
};


#' Create a new Classification Bar for a NGCHM
#'
#' This function is deprecated and will be removed in a future version.  Please use chmNewCovariateBar.
#' This function creates a new Classification Bar suitable for adding to a Next Generation Clustered Heat Map.
#'
#' @param label The name by which the classification bar will be known.
#' @param type The string "discrete" or the string "continuous".
#' @param data A vector of the data to be displayed in the classification bar. names(data) must be defined.
#' @param colors A color map specifying how the data should be rendered.
#' @param display Whether the classification bar will be "hidden" or "visible" (default).
#' @param thickness The thickness of the classification bar in pixels. (Default 10).
#' @param merge Algorithm for merging classifications when necessary ("average", "peakColor", "specialColor", or "mostCommon").
#' @param barType Type of covariate bar ("color_plot", "scatter_plot", "bar_plot"). Default "color_plot".
#' @param loBound Low bound for bar and scatter plots. Default minimum data value.
#' @param hiBound High bound for bar and scatter plots.  Default maximum data value.
#' @param fgColor Foreground color for bar and scatter plots.  Default black.
#' @param bgColor Background color for bar and scatter plots.  Default white.
#'
#' @return An object of class ngchmBar
#'
#' @seealso [ngchmBar-class]
#' @seealso [chmNewColorMap()]
#' @seealso [chmNewCovariateBar()]
#' @seealso [chmAddCovariateBar()]
#'
ngchmNewBar <- function (label, type, data, colors=NULL, display="visible", thickness=as.integer(10), merge, barType,
    loBound, hiBound, fgColor, bgColor) {
    if (typeof (label) != "character") {
        stop (sprintf ("Parameter 'label' must have type 'character', not '%s'", typeof(label)));
    }
    if (length (label) != 1) {
        stop (sprintf ("Parameter 'label' must have a single value, not %d", length(label)));
    }
    if (nchar (label) == 0) {
        stop ("Parameter 'label' cannot be the empty string");
    }
    if (typeof (type) != "character") {
        stop (sprintf ("Parameter 'type' for covariate bar '%s' must have type 'character', not '%s'", label, typeof(type)));
    }
    if (length (type) != 1) {
        stop (sprintf ("Parameter 'type' for covariate bar '%s' must have a single value, not %d", label, length(type)));
    }
    if (!(type %in% c("discrete", "continuous"))) {
        stop (sprintf ("Parameter 'type' for covariate bar '%s' must be either 'discrete' or 'continuous', not '%s'", label, type));
    }
    if (typeof (display) != "character") {
        stop (sprintf ("Parameter 'display' for covariate bar '%s' must have type 'character', not '%s'", label, typeof(display)));
    }
    if (length (display) != 1) {
        stop (sprintf ("Parameter 'display' for covariate bar '%s' must have a single value, not %d", label, length(display)));
    }
    if (!(display %in% c("visible", "hidden"))) {
        stop (sprintf ("Parameter 'display' for covariate bar '%s' must be either 'visible' or 'hidden', not '%s'", label, display));
    }
    if (missing(barType)) {
        barType <- "color_plot";
    }
    if (typeof (barType) != "character") {
        stop (sprintf ("Parameter 'barType' for covariate bar '%s' must have type 'character', not '%s'", label, typeof(barType)));
    }
    if (length (barType) != 1) {
        stop (sprintf ("Parameter 'barType' for covariate bar '%s' must have a single value, not %d", label, length(barType)));
    }
    if (!(barType %in% c("color_plot", "scatter_plot", "bar_plot"))) {
        stop (sprintf ("Parameter 'barType' for covariate bar '%s' must be either 'visible' or 'hidden', not '%s'", label, barType));
    }

    if (is (data, "shaid")) {
        shaid <- data;
        repo <- ngchmFindRepo (shaid);
        data <- ngchmLoadDatasetBlob (repo, shaid, if(type=='discrete') "" else 0.0)$mat[,'Value'];
    } else {
        shaid <- NULL;
    }
    if ((display == "visible") && (length(colors) == 0)) {
        if (type == "discrete") {
	    qq <- sort (unique (data));
	} else {
	    qq <- quantile (data);
	}
	colors <- grDevices::rainbow(length(qq),start=2/6,end=0);
	colors <- vapply (colors, function(cc)substr(cc,1,7), "");
	colors <- chmNewColorMap (qq, colors);
    }
    if (missing(merge)) {
        merge <- "average";
    }
    if ((length(merge) != 1) || !(merge %in% c("average", "peakColor", "specialColor", "mostCommon")))
        stop (sprintf ("Unknown covariate bar merge value '%s'. Must be 'average', 'peakColor', 'specialColor' or 'mostCommon'", merge));
    if (length(shaid)==0) {
        if (length(names(data)) == 0)
            stop (sprintf ("Parameter 'data' for covariate bar '%s' must have defined names that match those of the CHM axis to which it will be attached", label));
        if (anyDuplicated(names(data)) != 0) {
            dups <- unique(names(data)[which(duplicated(names(data)))]);
	stop (sprintf ("Parameter 'data' for covariate bar '%s' has multiple entries for label(s) %s", label, paste (dups, collapse=', ')));
        }
        data <- data[order(names(data))];
        mat <- matrix (data, ncol=1, dimnames=list(names(data),'Value'));
        shaid <- ngchmSaveAsDatasetBlob (ngchm.env$tmpShaidy, 'tsv', mat);
    }
    if (missing(loBound)) {
        loBound <- if (type == "continuous") min(data,na.rm=TRUE) else 0;
    }
    if (missing(hiBound)) {
        hiBound <- if (type == "continuous") max(data,na.rm=TRUE) else 1;
    }
    if (missing(fgColor)) {
        fgColor <- "#000000";
    }
    if (missing(bgColor)) {
        bgColor <- "#ffffff";
    }
    new (Class="ngchmBar", label=label, type=type, data=shaid, thickness=thickness, colors=colors, display=display, merge=merge, barType=barType, loBound=loBound, hiBound=hiBound, fgColor=fgColor, bgColor=bgColor)
};


#' Get a covariate bar attached to an NG-CHM.
#'
#' @param hm The NG-CHM to get the covariate bar from.
#' @param fullname The full name of the covariate bar to get.
#'        If no covariate bar with that name exists, return NULL.
#' @param where The axis or axes on which to look for the covariate bar  Can be "row", "column", or "both" (default).
#'
#' @return An ngchmBar or NULL.
#'
#' @export
#'
#' @examples
#' chmCovariateBar (hm, "Age")
#'
#' @seealso [ngchmBar-class]
#' @seealso chmNewCovariateBar
#' @seealso chmCovariate
#'
chmCovariateBar <- function (hm, fullname, where) {
    stopifnot (is (hm, "ngchm"));
    checkLabel (fullname, "fullname");
    where <- if (missing(where)) "both" else match.arg (where, c("row", "column", "both"));
    if (where %in% c("row", "both")) {
	names <- lapply (hm@rowCovariateBars, function(cvb) cvb@label);
	idx <- which (names == fullname);
	if (length(idx) > 0) {
	    stopifnot (length(idx) == 1);
	    return (hm@rowCovariateBars[[idx]]);
	}
	if (where == "row") return(NULL);
    }
    # where must be either "column" or "both".
    names <- lapply (hm@colCovariateBars, function(cvb) cvb@label);
    idx <- which (names == fullname);
    if (length(idx) > 0) {
	stopifnot (length(idx) == 1);
	return (hm@colCovariateBars[[idx]]);
    }
    return(NULL);
};



#' Create a new covariate Bar for a NGCHM
#'
#' This function creates a new covariate bar suitable for adding to a Next Generation Clustered Heat Map.
#'
#' @param covar The covariate to be displayed in the bar.
#' @param display Whether the covariate bar will be "hidden" or "visible" (default).
#' @param thickness The thickness of the covariate bar in pixels. (Default 10).
#' @param merge Algorithm for merging covariates when necessary ("average", "peakColor", "specialColor", or "mostCommon").
#' @param barType Type of covariate bar ("color_plot", "scatter_plot", "bar_plot"). Default "color_plot".
#' @param loBound Low bound for bar and scatter plots. Default minimum data value.
#' @param hiBound High bound for bar and scatter plots.  Default maximum data value.
#' @param fgColor Foreground color for bar and scatter plots.  Default black.
#' @param bgColor Background color for bar and scatter plots.  Default white.
#'
#' @return An object of class ngchmBar
#'
#' @export
#'
#' @examples
#' bar.data <- ifelse (rnorm(1000) < 0, "negative", "non-negative")
#' names(bar.data) <- sprintf ("Sample%d", 1:length(bar.data))
#' bar.colors <- chmNewColorMap (c("negative", "non-negative"),
#'                               c("white", "black"), missing.color='red')
#' covar <- chmNewCovariate ("Group", bar.data, bar.colors, "discrete")
#' bar <- chmNewCovariateBar (covar)
#'
#' @seealso [ngchmBar-class]
#' @seealso [chmNewColorMap()]
#' @seealso [chmAddCovariateBar()]
#'
chmNewCovariateBar <- function (covar, display="visible", thickness=as.integer(10), merge, barType,
	loBound, hiBound, fgColor, bgColor)
{
    ngchmNewBar (covar@fullname, covar@type, covar@label.series, covar@series.properties,
		    display=display, thickness=thickness, merge=merge, barType=barType,
		    loBound=loBound, hiBound=hiBound, fgColor=fgColor, bgColor=bgColor)
}

#' Create a new Color Map for use in constructing a NGCHM
#'
#' This function creates a new Color Map suitable for use in constructing Data Layers and Covariates
#' in Next Generation Clustered Heat Maps.  Color maps can be used in both discrete and continuous
#' contents.  In a discrete context, values specifies the properties of series.  In a continuous context,
#' values specifies the break points.
#'
#' If values is a matrix, the function will estimate a suitable sequence of color break points.  For a quantile
#' color map, the matrix data is ignored.  For a linear color map, it will use equispaced values between a low value and
#' a high value. The low value is the median of the minima of each row in the matrix, and the high value is the median
#' of the row maxima. If the low and high values have different signs, the values will be symmetric about zero.
#'
#' @param values A vector specifying the series / break points for which the following colors are defined, or a data matrix.
#' @param colors Either a string vector specifying the color to use for each series / break point, or a single integer.
#' @param names A string vector specifying 'human-readable' names for each series / break point.
#' @param shapes A string vector specifying the shape to use for each series.
#' @param zs A numeric vector specifying the z order to use for each series.
#' @param type The string "linear" (default) or "quantile" (or unique abbreviation thereof).
#' @param missing.color A string specifying the color to use for missing data.
#' @param palette A function(n) that returns a vector of n colors.
#'
#' @return An object of class ngchmColormap
#'
#' @export
#'
#' @examples
#' noise.colors <- chmNewColorMap (c(0,1,2),
#'                                 c("green", "black", "red"),
#'                                 missing.color="yellow")
#' bar.colors <- chmNewColorMap (c("small", "big"),
#'                               c("#00FFFF", "#FF00FF"),
#'                               type="quantile")
#'
#' @seealso [ngchmColormap-class]
#' @seealso [chmNewDataLayer()]
#' @seealso [chmNewCovariateBar()]
#'
chmNewColorMap <- function (values, colors=NULL, names=NULL, shapes=NULL, zs=NULL, type="linear", missing.color=NULL, palette=NULL) {
    # Validate parameter 'type'
    if (typeof (type) != "character") {
        stop (sprintf ("Parameter 'type' must have type 'character', not '%s'", typeof(type)));
    }
    if (length (type) != 1) {
        stop (sprintf ("Parameter 'type' must have a single value, not %d", length(type)));
    }
    allowedTypes = c("linear", "quantile");
    mtype = pmatch (type, allowedTypes);
    if (is.na(mtype)) {
        stop (sprintf ("Unknown color map type '%s'. Allowed types are %s.", type,
	               paste ("'", allowedTypes, "'", sep="", collapse=", ")));
    }
    type = allowedTypes[mtype];

    # Determine number of colors and whether auto-picking.
    NC <- length(colors);
    if ((NC == 1) && (mode(colors)=="numeric") && (as.integer(colors)==colors)) {
	if (colors < 2) {
	    stop (sprintf ('"%d" colors requested. At least two are required.', colors));
	}
        NC <- colors;
	colors <- NULL;
    }

    # Validate 'values'.  Auto-pick values if appropriate.
    if (isany(values,c('matrix','shaid'))) {
	# User just supplied a data matrix.
	if (NC == 0) NC <- 3;
        if (type == "quantile") {
	    values <- (1.0/(NC-1)) * (0:(NC-1));
	} else if (type == "linear") {
            if (is(values,"shaid")){
                # FIXME: redoing calculation is inefficient
                repo <- ngchmFindRepo (values);
	        values <- ngchmLoadDatasetBlob (repo, values)$mat;
            }
	    if ((nrow(values)==1) || (ncol(values)==1)) {
	        minv <- min (values, na.rm=TRUE);
	        maxv <- max (values, na.rm=TRUE);
	    } else {
		minv = median (apply (values, 1, function(v) min (v, na.rm=TRUE)), na.rm=TRUE);
		maxv = median (apply (values, 1, function(v) max (v, na.rm=TRUE)), na.rm=TRUE);
	    }
	    if ((minv < 0.0) && (maxv > 0.0)) {
		maxv <- max (maxv, -minv);
		minv <- -maxv;
	    }
	    values <- minv + (maxv - minv) * (1.0/(NC-1)) * (0:(NC-1));
	} else {
	    stop (sprintf ("chmNewColorMap: unable to derive color map breaks from matrix for map type '%s'", type));
	}
    } else if (isany(values, c('character', 'numeric', 'integer', 'logical'))) {
	# User supplied a vector of values.
	if (anyDuplicated(values) != 0) {
	    stop ("chmNewColorMap: values contains duplicates");
	}
	if (NC == 0) {
	    NC <- length (values);
	}
	if (length(values) != NC)
	    stop (sprintf ("chmNewColorMap: number of values (%d) does not equal number of color (%d). It should.", length(values), NC));
        if (isany(values, c('numeric','integer')) && !all(is.finite(values))) {
            stop('chmNewColorMap: values contains non-finite values');
        }
    } else {
	# Don't know what the user provided.
        stop (sprintf ("chmNewColorMap: values vector has unknown class '%s'. It must be either a vector or a matrix.",
	               classstr (values)));
    }
    # values is now a length NC vector of cut points
    stopifnot (length(values) == NC);

    # Auto-pick colors if needed.
    if (length(colors) == 0) {
	if (length(palette) > 0) {
	    colors <- palette(NC);
	} else {
	    colors <- grDevices::rainbow(NC,start=2/6,end=0);
	}
	colors <- vapply (colors, function(cc)substr(cc,1,7), "");
    }
    stopifnot (length(colors) == NC);

    # Validate other parameters.
    if (!is.null(names) && length(names)!=NC)
	stop (sprintf ("chmNewColorMap: number of names (%d) does not equal number of colors (%d). It should.", length(names), NC));
    if (!is.null(shapes) && length(shapes)!=NC)
	stop (sprintf ("chmNewColorMap: number of shapes (%d) does not equal number of colors (%d). It should.", length(shapes), NC));
    if (!is.null(zs) && length(zs)!=NC)
	stop (sprintf ("chmNewColorMap: number of zindices (%d) does not equal number of colors (%d). It should.", length(zs), NC));
    grDevices::col2rgb (missing.color);  # error check

    # Construct ValueMap
    pts <- chmAddValueProperty (NULL, value=values, color=colors, name=names, shape=shapes, z=zs);
    new (Class="ngchmColormap", type=type, missing=missing.color, points=pts)
}

chmAddValueProperty <- function (vps, value, color, name=NULL, shape=NULL, z=NULL) {
    some.shapes <- c("circle", "square", "diamond", "triangle");
    all.shapes <- c(some.shapes, "triangle-down", "line");
    if (is.null (name)) name <- as.character(value);
    if (is.null (shape)) shape <- some.shapes[1+(0:(length(value)-1)) %% length(some.shapes)];
    if (is.null (z)) z <- rep(9,length(value));
    if (!all(shape %in% all.shapes))
        stop ("unknown shape ", shape);
    if (any(z < 0))
        stop ("z must be non-negative");
    grDevices::col2rgb (color);  # error check
    for (ii in 1:length(value))
	vps <- append (vps, new (Class="ngchmValueProp", value=value[ii], color=color[ii], name=name[ii], shape=shape[ii], z=z[ii]));
    vps
}

#' Create a new Javascript function for adding to a NGCHM menu.
#'
#' This function creates a new Javascript function object for adding to
#' a Next Generation Clustered Heat Map menu.
#'
#' @param name The name of the Javascript function
#' @param description A short description of the Javascript function
#' @param implementation A string containing the javascript code required to define the function. When called the function
#'        is passed a list of selected values (e.g. labels).  Additional parameters can be declared before the values
#'        parameter and must be resolved through currying (binding) before the function is used in menus.
#' @param extraParams An optional list of extra parameters. (Default NULL.)
#' @param requires An optional vector of (custom) Javascript function names that this function requires.
#' @param global A logical: TRUE if should be defined globally, not within a customization section. (Default FALSE.)
#'
#' @return An object of class ngchmJS
#'
#' @export
#'
#' @examples
#' alertFn <- chmNewFunction ("showAlert", "Display the parameter in an alert box",
#'                            "function showAlert(label) { alert(label); }", global=TRUE)
#' dbLookup <- chmNewFunction ("dbLookup", "Lookup the parameter in a database",
#'                             "function showAlert(database, label) { alert(database[label]); }",
#'                             c("database"))
#'
#' @seealso [ngchmJS-class]
#' @seealso [chmAddMenuItem()]
#' @seealso [chmBindFunction()]
#' @seealso [chmRegisterFunction()]
#'
chmNewFunction <- function (name, description, implementation, extraParams=NULL, requires=NULL, global=FALSE) {
    if (typeof (name) != "character") {
        stop (sprintf ("Parameter 'name' must have type 'character', not '%s'", typeof(name)));
    }
    if (length (name) != 1) {
        stop (sprintf ("Parameter 'name' must have a single value, not %d", length(name)));
    }
    # We declare "" to be a simple value reference
    if (typeof (description) != "character") {
        stop (sprintf ("Parameter 'description' for JS function '%s' must have type 'character', not '%s'", name, typeof(description)));
    }
    if (length (description) != 1) {
        stop (sprintf ("Parameter 'description' for JS function '%s' must have a single value, not %d", name, length(description)));
    }
    if (nchar (description) == 0) {
        warning (sprintf ("Parameter 'description' for JS function '%s' should not be the empty string", name));
    }
    if (typeof (implementation) != "character") {
        stop (sprintf ("Parameter 'implementation' for JS function '%s' must have type 'character', not '%s'", name, typeof(implementation)));
    }
    if (length (implementation) != 1) {
        stop (sprintf ("Parameter 'implementation' for JS function '%s' must have a single value, not %d", name, length(implementation)));
    }
    # We might have an empty implementation if we are making R aware of a Javascript builtin
    if (length (extraParams) > 0) {
	if (typeof (extraParams) != "character") {
	    stop (sprintf ("Parameter 'extraParams' for JS function '%s' must have type 'character', not '%s'", name, typeof(extraParams)));
	}
    }
    if (length (requires) > 0) {
	if (typeof (requires) != "character") {
	    stop (sprintf ("Parameter 'requires' for JS function '%s' must have type 'character', not '%s'", name, typeof(requires)));
	}
    }
    fn <- new (Class="ngchmJS", name=name, description=description, script=implementation, requires=requires, extraParams=extraParams, global=global);
    chmRegisterFunction (fn)
}


checkLabel <- function (label, paramName = "label") {
    if (typeof (label) != "character") {
        stop (sprintf ("Parameter '%s' must have type 'character', not '%s'", paramName, typeof(label)));
    }
    if (length (label) != 1) {
        stop (sprintf ("Parameter '%s' must have a single value, not %d", paramName, length(label)));
    }
    if (nchar (label) == 0) {
        stop (sprintf ("Parameter '%s' cannot be the empty string", paramName));
    }
};

checkPropertyValue <- function (label, value) {
    if (!typeof (value) %in% c("character","double","integer","logical")) {
        stop (sprintf ("Parameter 'value' for property '%s' must have type 'character', 'double', 'integer', or 'logical', not '%s'", label, typeof(value)));
    }
    if (length (value) < 1) {
        stop (sprintf ("Parameter 'value' for property '%s' must have at least one value", label));
    }
};

#' Create a new Property for adding to a NGCHM.
#'
#' This function creates a new Property object for adding to
#' a Next Generation Clustered Heat Map.
#'
#' @param label The property label
#' @param value The property value
#'
#' @return An object of class ngchmProperty
#'
#' @export
#'
#' @examples
#' prop <- chmNewProperty ("chm.info.caption",
#'                            "This is a nifty new CHM.")
#'
#' @seealso [ngchm-class]
#' @seealso [chmAddProperty()]
#'
chmNewProperty <- function (label, value) {
    checkLabel(label);
    checkPropertyValue(label, value);
    new (Class="ngchmProperty", label=label, value=as.character(value))
}

#' Get the value of an NG-CHM property.
#'
#' @param hm The NG-CHM object to get the property value from.
#' @param label The name of the property to get.
#'        If no property with that name exists, return NULL.
#'
#'        Well-known property labels used by the NG-CHM system include:
#'
#'        * "chm.info.caption"  A paragraph describing the NG-CHM's contents (set by user).
#'	  * "chm.info.built.time"  The date and time the NG-CHM was saved (set by system).
#'
#' @return A property value or NULL.
#'
#' @export
#'
#' @examples
#' chmProperty (hm, "chm.info.caption")
#'
#' @seealso [ngchm-class]
#'
chmProperty <- function (hm, label) {
    stopifnot (is (hm, "ngchm"));
    checkLabel(label);
    labels <- lapply (hm@properties, function(p) p@label);
    idx <- which (label == labels);
    if (length(idx) == 0) return(NULL);
    stopifnot (length(idx) == 1);
    hm@properties[[idx]]@value
};

#' Set the value of an NG-CHM property.
#'
#' @param x The NG-CHM object on which to set the property.
#' @param label The name of the property to set.
#'        If no property with that name exists, a new property with that name is appended.
#' @param value A non-empty vector of character, logical, or numeric values.
#'
#' @return The modified NG-CHM object.
#'
#' @export
#'
#' @examples
#' chmProperty (hm, "chm.info.caption") <- "Nothing to see here";
#'
#' @seealso [ngchm-class]
#'
"chmProperty<-" <- function (x, label, value) {
    stopifnot (is (x, "ngchm"));
    checkLabel(label);
    checkPropertyValue(label, value);
    labels <- lapply (x@properties, function(p) p@label);
    idx <- which (label == labels);
    if (length(idx) == 0) {
	x@properties <- append (x@properties, new(Class="ngchmProperty", label=label, value=as.character(value)));
    } else {
	stopifnot (length(idx) == 1);
	x@properties[[idx]]@value <- as.character(value);
    }
    x
};

#' Create a list of NGCHM properties.
#'
#' Create a list of NGCHM properties.
#'
#' @param ... A named list of property values.
#'
#' @export
#' @seealso [chmAdd()]
chmProperties <- function (...) {
   props <- list(...);
   if (length(props) > 0) {
       if (is.null(names(props)) || any(names(props)=="")) {
	   stop ("All properties must be named");
       }
       props <- lapply(1:length(props), function(idx) {
	   label <- names(props)[idx];
	   value <- props[[idx]];
	   if (!typeof(value) %in% c("character","double","integer","logical")) {
	       stop (sprintf ("Property %s has invalid type %s", label, typeof(value)));
	   }
	   if (length(value) != 1) {
	       stop (sprintf ("Property %s has length %d", label, length(value)));
	   }
	   chmNewProperty (label, as.character(value))
       });
   }
   props
};

#' Get the label/name of an NG-CHM object.
#'
#' @param x The NG-CHM object to get the label/name of.  Can be:
#'
#' * An object of class ngchm
#' * An object of class ngchmLayer
#' * An object of class ngchmDataset
#' * An object of class ngchmBar
#' * An object of class ngchmCovariate
#' * An object of class ngchmColormap
#'
#' @return A character string (or a vector of strings for an ngchmColormap)
#'
#' @export
#'
#' @examples
#' chmLabel (chmNew('New CHM'))
#'
#' @seealso [ngchm-class]
#'
chmLabel <- function (x) {
    if (isany (x, c("ngchm", "ngchmLayer", "ngchmDataset"))) {
	return (x@name);
    }
    if (isany (x, c("ngchmBar", "ngchmCovariate"))) {
	return (x@label);
    }
    if (is (x, "ngchmColormap")) {
	return (vapply (x@points, function(p) p@name, ""));
    }
    stop ("Unknown object class");
};

#' Set the label/name of an NG-CHM object
#'
#' @param x The NG-CHM object on which to set the label/name.
#' @param value The new name (a single character string).
#'
#' @return The modified NG-CHM object.
#'
#' @export
#'
#' @examples
#' chmLabel (hm) <- "A new name";
#'
#' @seealso [chmLabel]
#'
"chmLabel<-" <- function (x, value) {
    stopifnot (isany (value, c("character", "numeric", "logical")),
               length(value) == 1 || is(x,"ngchmColormap"));
    value <- as.character (value);
    if (isany (x, c("ngchm", "ngchmLayer", "ngchmDataset"))) {
	x@name <- value;
    } else if (isany (x, c("ngchmBar", "ngchmCovariate"))) {
	x@label <- value;
    } else if (is (x, "ngchmColormap")) {
	stopifnot (length(x@points) == length(value));
	for (idx in 1:length(x@points)) x@points[[idx]]@name <- value[idx];
    } else {
	stop ("unknown object class");
    }
    x
};

#' Get the color map of an NG-CHM object.
#'
#' @param x The NG-CHM object to get the color map of.  Can be:
#'
#' * An object of class ngchmLayer
#' * An object of class ngchmBar
#' * An object of class ngchmCovariate
#'
#' @return An ngchmColormap
#'
#' @export
#'
#' @examples
#' data(TCGA.GBM.EXPR, package='NGCHMDemoData');
#' chmColorMap (chmNewDataLayer('New layer', TCGA.GBM.EXPR[1:3,1:3]))
#'
#' @seealso [chmNewColorMap]
#'
chmColorMap <- function (x) {
    if (isany(x, c("ngchmLayer", "ngchmBar"))) {
	return (x@colors);
    }
    if (is(x, "ngchmCovariate")) {
	return (x@series.properties);
    }
    stop ("Unknown object class");
};

#' Set the color map of an NG-CHM object
#'
#' @param x The NG-CHM object on which to set the color map.
#' @param value The ngchmColormap value to set.
#'
#' @return The modified NG-CHM object.
#'
#' @export
#'
#' @examples
#' data(TCGA.GBM.EXPR, package='NGCHMDemoData');
#' chmColorMap (chmNewDataLayer('New layer', TCGA.GBM.EXPR[1:3,1:3]), chmNewColormap (c(2,14));
#'
#' @seealso [chmColorMap]
#'
"chmColorMap<-" <- function (x, value) {
    stopifnot (is (value, "ngchmColormap"));
    if (isany(x, c("ngchmLayer", "ngchmBar"))) {
	x@colors <- value;
    } else if (is(x, "ngchmCovariate")) {
	x@series.properties <- value;
    } else {
	stop ("Unknown object class");
    }
    x
};

#' Get the colors of an ngchmColormap, ngchmLayer, ngchmBar, or ngchmCovariate.
#'
#' @param x The object to get the colors of.
#'
#' @return A character string vector of the map colors.
#'
#' @export
#'
#' @examples
#' data(TCGA.GBM.EXPR, package='NGCHMDemoData');
#' chmColors (chmNewDataLayer('New Layer', TCGA.GBM.EXPR[1:50,1:50))
#'
#' @seealso [ngchm-class]
#'
chmColors <- function (x) {
    if (!is(x, "ngchmColormap")) x <- chmColorMap(x);
    vapply (x@points, function(p) p@color, "")
};

#' Set the colors of an ngchmColormap, ngchmLayer, ngchmBar, or ngchmCovariate.
#'
#' @param x The NG-CHM object on which to set the colors.
#' @param value A character string vector of colors.  The vector length must equal
#'        the number of data points in the color map.
#'
#' @return The modified NG-CHM object.
#'
#' @export
#'
#' @examples
#' chmColors (layer) <- c("blue", "white", "red")
#'
#' @seealso [chmColors]
#'
"chmColors<-" <- function (x, value) {
    stopifnot (is (value, "character"));
    cm <- if (is(x,"ngchmColormap")) x else chmColorMap(x);
    stopifnot (length(cm@points) == length(value));
    for (idx in 1:length(cm@points)) cm@points[[idx]]@color <- value[idx];
    if (is(x, "ngchmColormap")) {
	return (cm);
    } else {
	chmColorMap(x) <- cm;
	x
    }
};

#' Create a new object representing a NGCHM server.
#'
#' This function creates a new object that represents a NGCHM server.
#'
#' @param serverName The DNS name of the NGCHM server.
#' @param serverPort The port on which the server is listening.
#' @param deployServer The DNS name to use when deploying a NGCHM (defaults to serverName).
#' @param protoOpts A list of protocol-specific parameters
#' @param jarFile The location of the heatmap build jar file to use when making a NGCHM (defaults to jar file on serverURL WS).
#' @param serverURL The URL used to access the NGCHM server (defaults to serverName:serverPort/chm).
#'
#' @return An object of class ngchmServer
#'
#' @export
#'
#' @examples
#' cloudServ <- chmNewServer ("dnsname.domain")
#'
#' @seealso [ngchmServer-class]
#' @seealso [chmInstall()]
#' @seealso [chmUninstall()]
#'
chmNewServer <- function (serverName, serverPort=8080, deployServer=NULL, protoOpts=NULL,
                       jarFile=NULL, serverURL=NULL)
{
    if (is.null (deployServer)) deployServer = serverName;
    if (is.null (serverURL)) serverURL = paste ("http://", serverName, ":", serverPort, "/chm", sep="");
    new (Class="ngchmServer",
	 deployServer = deployServer,
	 protoOpts = protoOpts,
	 jarFile = jarFile,
	 serverURL = serverURL);
}

#############################################################################################
#
# Javascript Customization.
#

#' Get a predefined Javascript function for use in NGCHM menus
#'
#' This function returns a predefined Javascript function that can be used when
#' building a Next Generation Clustered Heat Map.
#'
#' @param name The name of the predefined Javascript function desired.
#'
#' @return An object of class ngchmFunction if found, NULL otherwise.
#'
#' @export
#'
#' @seealso [chmAddMenuItem()]
#' @seealso [chmNewFunction()]
#' @seealso [ngchmAxisFunction-class]
#' @seealso [ngchmMatrixFunction-class]
#'
chmGetFunction <- function (name) {
    if (typeof (name) != "character") {
        stop (sprintf ("Parameter 'name' must have type 'character', not '%s'", typeof(name)));
    }
    if (length (name) != 1) {
        stop (sprintf ("Parameter 'name' must have a single value, not %d", length(name)));
    }
    if (is.list (ngchm.env$scripts)) {
        for (ii in 1:length(ngchm.env$scripts))
	    if (ngchm.env$scripts[[ii]]@name == name)
	        return (ngchm.env$scripts[[ii]]);
    }
    return (NULL);
}

#' Register a predefined Javascript function for use in NGCHM Axis menus.
#'
#' This function registers a Javascript function that will be automatically
#' added to the appropriate axis menu(s) when building a Next Generation
#' Clustered Heat Map for axes that match the function's axis type.
#' This function is intended for use by NGCHM system developers.
#'
#' @param type The axis type required by this function.
#' @param label The name of the axis menu entry to be used for this function.
#' @param fn The Javascript function to register.
#'
#' @return NULL
#'
#' @export
#'
#' @seealso [chmAddAxisType()]
#' @seealso [chmRegisterMatrixFunction()]
#' @seealso [chmRegisterTypeMapper()]
#' @seealso [chmNewFunction()]
chmRegisterAxisFunction <- function (type, label, fn) {
    if (typeof (label) != "character") {
        stop (sprintf ("Parameter 'label' must have type 'character', not '%s'", typeof(label)));
    }
    if (length (label) != 1) {
        stop (sprintf ("Parameter 'label' must have a single value, not %d", length(label)));
    }
    if (nchar (label) == 0) {
        stop ("Parameter 'label' cannot be the empty string");
    }
    if (typeof (type) != "character") {
        stop (sprintf ("Parameter 'type' for axis function '%s' must have type 'character', not '%s'", label, typeof(type)));
    }
    if (length (type) < 1) {
        stop (sprintf ("Parameter 'type' for axis function '%s' must have at least one value", label));
    }
    for (ii in 1:length(type)) {
	if (nchar (type[ii]) == 0) {
	    stop (sprintf ("Parameter 'type[%d]' for axis function '%s' cannot be the empty string", ii, label));
	}
    }
    if (!is(type, "character"))
        stop (sprintf ("chmRegisterAxisFunction: error registering axis function '%s'. Specified type is '%', not string.", label, classstr(type)));
    if (!is(label, "character"))
        stop (sprintf ("chmRegisterAxisFunction: error registering axis function. Type of specified label is '%', not string.", classstr(type)));
    if (is(fn, "character"))
        fn <- chmGetFunction (fn);
    af <- new ("ngchmAxisFunction", type=type, label=label, func=fn);
    matches <- which (vapply (ngchm.env$axisFunctions, function(af) (af@label == label) && (af@type == type), TRUE));
    if (length (matches) > 0) {
	ngchm.env$axisFunctions[[matches]] <- af;
    } else {
	ngchm.env$axisFunctions <- append (ngchm.env$axisFunctions, af);
    }
    NULL
}

getAllAxisTypes <- function (chm, where) {
    w <- vapply (chm@axisTypes, function(at)at@where, "");
    locs <- (w == where) | (w == "both");
    # types is a list of character vectors (since an axistype can have > 1 type)
    types <- lapply (chm@axisTypes[locs], function(at)at@type);
    uu <- !duplicated(types);
    types <- types[uu];
    builders <- chm@axisTypes[locs][uu];

    # fromtypes is a list of character vectors (since a mapper can have > 1 fromtype)
    fromtypes <- lapply (ngchm.env$typeMappers, function(mm)mm@fromtype);
    # totypes is a character vector (since each mapper has exactly 1 totype)
    totypes <- vapply (ngchm.env$typeMappers, function(mm)mm@totype, "");
    ii <- 0;
    while (ii < length(types)) {
        ii = ii + 1;
	# Find those fromtypes that match the current type.
	extra <- vapply (fromtypes, function(ft)any(vapply(types[[ii]], function(tt)any(tt==ft), TRUE)), TRUE);
	# Append types of matching mappers, but exclude any duplicates.
	# FIXME: only excludes exact duplicate type lists. Does not exclude a mapper if all its types can
	# be obtained from a collection of the current types.
	types <- append (types, totypes[extra]);
	uu <- !duplicated(types);
	types <- types[uu];
	builders <- append(builders, ngchm.env$typeMappers[extra])[uu];
    }
    return (list(types=types, builders=builders));
}

getAllAxisFunctions <- function (chm, where) {
    w <- vapply (chm@axisTypes, function(at)at@where, "");
    locs <- (w == where) | (w == "both");
    fns <- lapply (chm@axisTypes[locs], function(at)at@func);
    return (fns);
}

# axistypes is a list of character vectors (since an axis type can have > 1 type)
getAllAxisTypeFunctions <- function (chmSpecificFns, axistypes) {
    fnList <- append (chmSpecificFns, ngchm.env$axisFunctions);
    matches <- vapply (fnList,
                       function(af)any(vapply (axistypes, function(at)any(at==af@type), TRUE)),
		       TRUE);
    return (fnList[matches]);
}

getAllMatrixTypeFunctions <- function (chm, rowtypes, columntypes) {
    matches <- vapply (ngchm.env$matrixFunctions,
                      function(mf) {
			  rowm <- any(vapply (rowtypes, function(at)any(at==mf@rowtype), TRUE));
			  colm <- any(vapply (columntypes, function(at)any(at==mf@columntype), TRUE));
			  cat ("getAllMatrixTypeFunctions for ", mf@label, ":\n", file=stderr());
			  cat ("    row matches: ", rowm, "\n", file=stderr());
			  cat ("    col matches: ", colm, "\n", file=stderr());
			  rowm && colm
		       },
		      TRUE);
    cat ("getAllMatrixTypeFunctions: found ", sum(matches), " matches.\n", file=stderr());
    return (ngchm.env$matrixFunctions[matches]);
}

#' Register a predefined Javascript function for use in NGCHM Matrix menus.
#'
#' This function registers a Javascript function that will be automatically
#' added to the matrix menu when building a Next Generation
#' Clustered Heat Map for matrices whose rows and columns match then function's
#' axes types.
#' This function is intended for use by NGCHM system developers.
#'
#' @param rowtype The row type required by this function.
#' @param columntype The column type required by this function.
#' @param label The name of the axis menu entry to be used for this function.
#' @param fn The Javascript function to register.
#'
#' @return NULL
#'
#' @export
#'
#' @seealso [chmAddAxisType()]
#' @seealso [chmRegisterAxisFunction()]
#' @seealso [chmRegisterTypeMapper()]
#' @seealso [chmNewFunction()]
chmRegisterMatrixFunction <- function (rowtype, columntype, label, fn) {
    if (typeof (label) != "character") {
        stop (sprintf ("Parameter 'label' must have type 'character', not '%s'", typeof(label)));
    }
    if (length (label) != 1) {
        stop (sprintf ("Parameter 'label' must have a single value, not %d", length(label)));
    }
    if (nchar (label) == 0) {
        stop ("Parameter 'label' cannot be the empty string");
    }
    if (typeof (rowtype) != "character") {
        stop (sprintf ("Parameter 'rowtype' for matrix function '%s' must have type 'character', not '%s'", label, typeof(rowtype)));
    }
    if (length (rowtype) < 1) {
        stop (sprintf ("Parameter 'rowtype' for matrix function '%s' must have at least one value", label));
    }
    if (typeof (columntype) != "character") {
        stop (sprintf ("Parameter 'columntype' for matrix function '%s' must have type 'character', not '%s'", label, typeof(columntype)));
    }
    if (length (columntype) < 1) {
        stop (sprintf ("Parameter 'columntype' for matrix function '%s' must have at least one value", label));
    }
    if (is(fn, "character"))
        fn <- chmGetFunction (fn);
    newmf <- new ("ngchmMatrixFunction", rowtype=rowtype, columntype=columntype, label=label, func=fn);
    matches <- which (vapply (ngchm.env$matrixFunctions, function(mf) (mf@label == label) && (mf@rowtype == rowtype) && (mf@columntype == columntype), TRUE));
    if (length (matches) > 0) {
	ngchm.env$matrixFunctions[[matches]] <- newmf;
    } else {
	ngchm.env$matrixFunctions <- append (ngchm.env$matrixFunctions, newmf);
    }
    NULL
}

#' Register a type name.
#'
#' This function registers a type name used for determining row and column
#' linkouts.
#' This function is intended to be used by NGCHM system developers to record
#' basic information about the semantic interpretation of a type name.  Registration
#' of a typename is (currently) not required in order to use it.
#'
#' @param typename The name of the type.
#' @param description A description of the type.
#'
#' @return NULL
#'
#' @export
#'
#' @seealso [chmListTypes()]
#' @seealso [chmGetTypeInfo()]
#' @seealso [chmRegisterTypeMapper()]
chmRegisterType <- function (typename, description) {
    if (typeof (typename) != "character") {
        stop (sprintf ("Parameter 'typename' must have type 'character', not '%s'", typeof(typename)));
    }
    if (typeof (description) != "character") {
        stop (sprintf ("Parameter 'description' must have type 'character', not '%s'", typeof(description)));
    }
    if (length(typename) < 1)
        stop ("chmRegisterType: at least one typename must be specified.");
    if (length(description) != 1)
        stop (sprintf ("chmRegisterType: description must be exactly a single string, not %d.", length(description)));
    for (ii in 1:length(typename)) {
	tt <- data.frame (name=typename[ii], description=description, stringsAsFactors=FALSE);
	if (typename[ii] %in% ngchm.env$typeInfo$name) {
	    warning (sprintf ('Replacing definition of typename "%s"', typename[ii]));
	    idx <- which (ngchm.env$typeInfo$name == typename[ii]);
	    ngchm.env$typeInfo[idx,] <- tt;
	} else {
	    ngchm.env$typeInfo <- rbind (ngchm.env$typeInfo, tt);
	}
    }
    NULL
}

#' Get information about a type name.
#'
#' This function gets any registered information about a type name used for determining row and column
#' linkouts.
#' Registration of a typename is (currently) not required in order to use it, so it's possible for
#' valid type name not to have any registered information.
#'
#' @param typename The name of the type.
#'
#' @return A list containing basic information about the type.
#'
#' @export
#'
#' @seealso [chmListTypes()]
#' @seealso [chmRegisterType()]
chmGetTypeInfo <- function (typename) {
    if (typeof (typename) != "character") {
        stop (sprintf ("Parameter 'typename' must have type 'character', not '%s'", typeof(typename)));
    }
    if (length(typename) != 1) {
        stop ("chmGetTypeInfo: exactly one typename must be specified.");
    }
    if (!(typename %in% ngchm.env$typeInfo$name)) {
	typeinfo <- list (name=typename, description=NULL);
    } else {
	idx <- which (ngchm.env$typeInfo$name == typename);
	typeinfo <- as.list (ngchm.env$typeInfo[idx,]);
    }

    # Find any axis functions that match
    matches <- which (vapply (ngchm.env$axisFunctions, function(af) any(af@type == typename), TRUE));
    if (length (matches) > 0) {
	typeinfo$axisFunctions <- ngchm.env$axisFunctions[matches];
    } else {
	typeinfo$axisFunctions <- NULL;
    }

    # Find any matrix functions that match
    matches <- which (vapply (ngchm.env$matrixFunctions, function(af) any(af@rowtype == typename) || any(af@columntype == typename), TRUE));
    if (length (matches) > 0) {
	typeinfo$matrixFunctions <- ngchm.env$matrixFunctions[matches];
    } else {
	typeinfo$matrixFunctions <- NULL;
    }

    # Find any type mappers that match
    matches <- which (vapply (ngchm.env$typeMappers, function(af) any(af@fromtype == typename), TRUE));
    if (length (matches) > 0) {
	typeinfo$typeMappers <- ngchm.env$typeMappers[matches];
    } else {
	typeinfo$typeMappers <- NULL;
    }

    class(typeinfo) <- "ngchm.type.info";
    typeinfo
}

#' Pretty print the result returned by chmGetTypeInfo.
#'
#' @param x Type information about an NGCHM type name as return by chmGetTypeInfo.
#' @param ... Generic function accepts additional parameters
#'
#' @export
#'
#' @seealso [chmGetTypeInfo()]
print.ngchm.type.info <- function (x, ...) {
    cat (sprintf ("NGCHM type %s: %s\n", x$name, x$description));
    if (length (x$axisFunctions) > 0) {
	fns <- paste (vapply (x$axisFunctions, function(af)af@label, ""), collapse=", ");
	cat (sprintf ("matches axis functions %s.\n", fns));
    }
    if (length (x$matrixFunctions) > 0) {
	fns <- paste (vapply (x$matrixFunctions, function(af)af@label, ""), collapse=", ");
	cat (sprintf ("matches matrix functions %s.\n", fns));
    }
    if (length (x$typeMappers) > 0) {
	types <- paste (vapply (x$typeMappers, function(af)af@totype, ""), collapse=", ");
	cat (sprintf ("maps to types %s.\n", types));
    }
}

#' Register a predefined Javascript function for converting values from
#' one type to another.
#'
#' This function registers a Javascript function that will be automatically
#' added to a Next Generation Clustered Heat Map as required for converting
#' values from one type into another more basic type.
#' This function is intended for use by NGCHM system developers.
#'
#' @param fromtype The type of values the function expects as input.
#' @param totype The type of values the function will produce.  The length of
#'        totype must be shorter than fromtype.
#' @param op The operation code for performing the conversion
#' @param ... Additional parameters required for specifying the conversion (op specific)
#'
#' @return NULL
#'
#' op can have the following values:
#' + 'field' Split source into fields separated by 'separator' and select field 'num' (0 origin)
#' + 'expr' Compute string expression 'expr'. 'return' value is a vector or a scalar (default)
#' + 'javascript' Evaluate javascript function 'fn' (deprecated)
#'
#' @export
#'
#' @seealso [chmAddAxisType()]
#' @seealso [chmRegisterAxisFunction()]
#' @seealso [chmRegisterMatrixFunction()]
#' @seealso [chmNewFunction()]
chmRegisterTypeMapper <- function (fromtype, totype, op, ...) {
    if (typeof (fromtype) != "character") {
        stop (sprintf ("Parameter 'fromtype' must have type 'character', not '%s'", typeof(fromtype)));
    }
    if (typeof (totype) != "character") {
        stop (sprintf ("Parameter 'totype' must have type 'character', not '%s'", typeof(totype)));
    }
    if (length(fromtype) < 1)
        stop ("chmRegisterTypeMapper: at least one fromtype must be specified.");
    if (length(totype) != 1)
        stop (sprintf ("chmRegisterTypeMapper: totype must be exactly a single string, not %d.", length(totype)));
    for (ii in 1:length(fromtype))
	if (nchar(totype) >= nchar(fromtype[ii]))
	    stop (sprintf ("chmRegisterTypeMapper: totype ('%s') must be shorter than fromtype ('%s').", totype, fromtype[ii]));
    stopifnot (is(op, "character") && length(op)==1);

    pp <- list (...);
    if (op == "field") {
        stopifnot ("separator" %in% names(pp));
        stopifnot (is(pp$separator,"character") && length(pp$separator)==1);
        if (!"num" %in% names(pp)) pp$num <- 0;
        params <- list(separator=pp$separator, num=pp$num);
    } else if (op == "expr") {
        stopifnot ("expr" %in% names(pp));
        stopifnot (is(pp$expr,"character") && length(pp$expr)==1);
        if (!"return" %in% names(pp)) pp$return <- 'scalar';
        params <- list(expr=pp$expr, return=pp$return);
    } else if (op == "javascript") {
        stopifnot ("fn" %in% names(pp));
        if (is(pp$fn, "character")) {
            pp$fn <- chmGetFunction (pp$fn);
        }
        params <- list(fn=pp$fn);
    } else {
        stop ("unknown type mapper op");
    }
    newtm <- new ("ngchmTypeMapper", fromtype=fromtype, totype=totype, op=op, params=params);
    matches <- which (vapply (ngchm.env$typeMappers, function(tm) (length(intersect (tm@fromtype, fromtype)) > 0) && (tm@totype == totype), TRUE));
    if (length(matches) > 0) {
	matches <- sort (matches, decreasing=TRUE);
	for (idx in matches) {
	    ftype <- ngchm.env$typeMappers[[idx]]@fromtype;
	    common <- intersect(ftype, fromtype);
	    if (length(common) == length(ftype)) {
	        # All fromtypes are replaced: remove mapping.
		ngchm.env$typeMappers <- ngchm.env$typeMappers[-idx];
	    } else {
		ngchm.env$typeMappers[[idx]]@fromtype <- setdiff (ftype, common);
	    }
	}
    }
    ngchm.env$typeMappers <- append (ngchm.env$typeMappers, newtm);
    NULL
}

#' Register a predefined Javascript function for use in NGCHM menus.
#'
#' This function registers a Javascript function that can be used when
#' building a Next Generation Clustered Heat Map.  This function is
#' intended for use by NGCHM system developers.
#'
#' @param fn The Javascript function to register.
#'
#' @return NULL
#'
#' @export
#'
#' @seealso [chmAddMenuItem()]
#' @seealso [chmNewFunction()]
#' @seealso [ngchmAxisFunction-class]
#' @seealso [ngchmMatrixFunction-class]
#'
chmRegisterFunction <- function (fn) {
    if (!is(fn, "ngchmJS")) {
        stop (sprintf ("Parameter 'fn' must have type 'ngchmJS', not '%s'", classstr(fn)));
    }
    matches <- which (vapply (ngchm.env$scripts, function(ss) (ss@name == fn@name), TRUE));
    if (length (matches) > 0) {
	ngchm.env$scripts[[matches]] <- fn;
    } else {
	ngchm.env$scripts <- append (ngchm.env$scripts, fn);
    }
    fn
}

#' Create and register an NGCHM server protocol implementation.
#'
#' This function creates and registers a protocol implementation for manipulating
#' an NGCHM server.
#'
#' @param protocolName The name of this protocol implementation.
#' @param chmFormat The chm format required by this protocol. Defaults to original format.
#' @param requiredParams The protocol's required parameters, if any.
#' @param optionalParams The protocol's optional parameters, if any.
#' @param paramValidator A function(list) for validating the parameters specified for a new server.
#' @param findCollection A function(server,collection,path) for finding a collection.
#' @param createCollection A function(server,collection,name) for creating a collection.
#' @param installMethod A function(server,chm) for installing an NG-CHM.
#' @param uninstallMethod A function(server,chmname) for uninstalling an NG-CHM.
#' @param makePrivate A function(server,chmname) for hiding an NG-CHM.
#' @param makePublic A function(server,chmname) for showing an NG-CHM.
#' @param setCredentials A function(server,credentialstring) for setting credentials.
#'
#' @export

ngchmCreateServerProtocol <- function (protocolName, chmFormat,
				       requiredParams, optionalParams,
				       paramValidator,
                                       findCollection, createCollection,
                                       installMethod, uninstallMethod,
	                               makePrivate, makePublic,
                                       setCredentials) {
    if (missing(chmFormat)) chmFormat <- "original";
    if (typeof (chmFormat) != "character") {
        stop (sprintf ("Parameter 'chmFormat' must have type 'character', not '%s'", typeof(chmFormat)));
    }
    if (length (chmFormat) != 1) {
        stop (sprintf ("Parameter 'chmFormat' must have a single value, not %d", length(chmFormat)));
    }
    if (nchar (chmFormat) == 0) {
        stop ("Parameter 'chmFormat' cannot be the empty string");
    }
    if (typeof (protocolName) != "character") {
        stop (sprintf ("Parameter 'protocolName' must have type 'character', not '%s'", typeof(protocolName)));
    }
    if (length (protocolName) != 1) {
        stop (sprintf ("Parameter 'protocolName' must have a single value, not %d", length(protocolName)));
    }
    if (nchar (protocolName) == 0) {
        stop ("Parameter 'protocolName' cannot be the empty string");
    }
    if (missing (requiredParams)) {
        requiredParams <- NULL;
    }
    if (!is.null(requiredParams) && typeof (requiredParams) != "character") {
        stop (sprintf ("Parameter 'requiredParams' must have type 'character', not '%s'", typeof(requiredParams)));
    }
    if (missing (optionalParams)) {
        optionalParams <- NULL;
    }
    if (!is.null(optionalParams) && typeof (optionalParams) != "character") {
        stop (sprintf ("Parameter 'optionalParams' must have type 'character', not '%s'", typeof(optionalParams)));
    }
    if (missing (paramValidator)) {
        paramValidator <- function (params) {
        };
    }
    if (missing (findCollection)) {
	findCollection <- function (server, collectionId, path) {
	    return (NULL);
	};
    }
    if (missing (createCollection)) {
	createCollection <- function (server, collectionId, name) {
	    return (NULL);
	};
    }
    if (missing (installMethod)) {
	installMethod <- function (server, chm) {
	    stop ("NGCHMs cannot be automatically installed on this server. Please obtain installation instructions from the server administrator.");
        };
    }
    if (missing (uninstallMethod)) {
	uninstallMethod <- function (server, chmname) {
	    stop ("NGCHMs cannot be automatically uninstalled from this server. Please obtain instructions from the server administrator.");
        };
    }
    if (missing (makePrivate)) {
        makePrivate <- function (server, chmname) {
	    stop ("NGCHMs cannot be automatically made private on this server. Please obtain instructions from the server administrator.");
        };
    }
    if (missing (makePublic)) {
        makePublic <- function (server, chmname) {
	    stop ("NGCHMs cannot be automatically made public on this server. Please obtain instructions from the server administrator.");
        }
    }
    if (missing (setCredentials)) {
        setCredentials <- function (server, credentialstring) {
	    stop ("You cannot set credentials for this server. Please obtain instructions from the server administrator.");
        }
    }
    dm <- new (Class="ngchmServerProtocol", protocolName=protocolName, chmFormat=chmFormat,
	       requiredParams = requiredParams,
	       optionalParams = optionalParams,
	       paramValidator = paramValidator,
               setCredentials = setCredentials,
               findCollection = findCollection, createCollection = createCollection,
	       installMethod = installMethod, uninstallMethod = uninstallMethod,
	       makePrivate = makePrivate, makePublic = makePublic);
    matches <- which (vapply (ngchm.env$serverProtocols, function(ss) (ss@protocolName == protocolName), TRUE));
    if (length (matches) > 0) {
	ngchm.env$serverProtocols[[matches]] <- dm;
    } else {
	ngchm.env$serverProtocols <- append (ngchm.env$serverProtocols, dm);
    }
    dm
}

#' List defined server protocols
#'
#' @export
#'
#' @return A character vector

ngchmListServerProtocols <- function () {
    vapply (ngchm.env$serverProtocols, function(sp) sp@protocolName, '')
}

#' Lookup a Server Protocol
#'
#' @export
#'
#' @param protocolName The name of the server protocol to lookup

ngchmGetServerProtocol <- function (protocolName) {
    if (typeof (protocolName) != "character") {
        stop (sprintf ("Parameter 'protocolName' must have type 'character', not '%s'", typeof(protocolName)));
    }
    if (length (protocolName) != 1) {
        stop (sprintf ("Parameter 'protocolName' must have a single value, not %d", length(protocolName)));
    }
    if (nchar (protocolName) == 0) {
        stop ("Parameter 'protocolName' cannot be the empty string");
    }
    matches <- which (vapply (ngchm.env$serverProtocols, function(sp) (sp@protocolName == protocolName), TRUE));
    if (length(matches) == 0) {
        stop (sprintf ("No server protocol found with name '%s'", protocolName));
    } else {
	return (ngchm.env$serverProtocols[[matches]]);
    }
}

#' Get a server protocol parameter
#'
#' @export
#'
#' @param server The NGCHM server
#' @param option The name of the protocol parameter to get
#' @param default The default value of the option (default NULL)

ngchmGetProtoParam <- function (server, option, default=NULL) {
    stopifnot (is (server, "ngchmServer"));
    stopifnot (is (option, "character"));
    stopifnot (length(option) == 1);
    if (is.null (server@protoOpts)) return (default);
    if (option %in% names(server@protoOpts)) return (server@protoOpts[[option]]);
    return (default);
}

#' List the predefined Javascript functions available for use in NGCHM menus.
#'
#' This function lists the predefined Javascript functions available for use in NGCHM menus.
#'
#' @param re Only functions with names matching re are printed (default ".*")
#'
#' @export
#'
#' @seealso [chmAddMenuItem()]
#' @seealso [chmGetFunction()]
#' @seealso [chmRegisterFunction()]
#' @seealso [grep()]
#'
chmListFunctions <- function (re=".*") {
    if (typeof (re) != "character") {
        stop (sprintf ("Parameter 're' must have type 'character', not '%s'", typeof(re)));
    }
    for (ii in 1:length(ngchm.env$scripts)) {
	fn <- ngchm.env$scripts[[ii]];
        if (length (grep (re, fn@name)) > 0) {
            cat (sprintf ("%s\t%s\n", fn@name, fn@description));
	}
    }
}

#' Define and register a Javascript function for obtaining a specific metadata value.
#'
#' This function defines and registers a Javascript function for obtaining a specific
#' metadata value and returning it as a javascript list.  The function is suitable for
#' use as an axis type accessor function.
#'
#' @param functionName The name given to the Javascript function.
#' @param metadataColumnName The name of the metadata column to access.
#'
#' @export
#'
#' @seealso [chmAddAxisType()]
#' @seealso [chmGetFunction()]
#' @seealso [chmListFunctions()]
#'
chmRegisterGetMetadataFunction <- function (functionName, metadataColumnName) {
    if (typeof (functionName) != "character") {
        stop (sprintf ("Parameter 'functionName' must have type 'character', not '%s'", typeof(functionName)));
    }
    if (length (functionName) != 1) {
        stop (sprintf ("Parameter 'functionName' must have a single value, not %d", length(functionName)));
    }
    if (nchar (functionName) == 0) {
        stop ("Parameter 'functionName' cannot be the empty string");
    }
    if (typeof (metadataColumnName) != "character") {
        stop (sprintf ("Parameter 'metadataColumnName' must have type 'character', not '%s'", typeof(metadataColumnName)));
    }
    if (length (metadataColumnName) != 1) {
        stop (sprintf ("Parameter 'metadataColumnName' must have a single value, not %d", length(metadataColumnName)));
    }
    if (nchar (metadataColumnName) == 0) {
        stop ("Parameter 'metadataColumnName' cannot be the empty string");
    }
    chmNewFunction (functionName,
        "This returns the label at the specified index as a list of values.  Can be used whenever the label itself is of the correct type.",
	paste (sprintf ("function %s (axis, idx) {", functionName),
	       sprintf ("    return [axis.labels.getMetaData (idx).%s];", metadataColumnName),
	       "};", sep="\n"));
}

#' Define and register a Javascript function for converting a lists of type values into single values.
#'
#' This function defines and registers a Javascript function for converting a list of type values
#' separated by the specified separator into the single values, and registers it as a type mapper.
#'
#' @param functionName The name given to the Javascript function.
#' @param listtype The name of the list type.
#' @param itemtype The name of the individual values.
#' @param separator The string that separates items within the list.
#'
#' @export
#'
#' @seealso [chmGetFunction()]
#' @seealso [chmListFunctions()]
#' @seealso [chmRegisterTypeMapper()]
#'
chmRegisterTypeSplitter <- function (functionName, listtype, itemtype, separator) {
    if (typeof (functionName) != "character") {
        stop (sprintf ("Parameter 'functionName' must have type 'character', not '%s'", typeof(functionName)));
    }
    if (length (functionName) != 1) {
        stop (sprintf ("Parameter 'functionName' must have a single value, not %d", length(functionName)));
    }
    if (nchar (functionName) == 0) {
        stop ("Parameter 'functionName' cannot be the empty string");
    }
    if (typeof (separator) != "character") {
        stop (sprintf ("Parameter 'separator' for typesplitter '%s' must have type 'character', not '%s'", functionName, typeof(separator)));
    }
    if (length (separator) != 1) {
        stop (sprintf ("Parameter 'separator' for typesplitter '%s' must have a single value, not %d", functionName, length(separator)));
    }
    if (nchar (separator) == 0) {
        stop (sprintf ("Parameter 'separator' for typesplitter '%s' cannot be the empty string", functionName));
    }
    fn <- chmNewFunction (functionName,
             sprintf ("Convert %s to %s by splitting on '%s'", listtype, itemtype, separator),

	     paste (sprintf ("function %s (names) {", functionName),
		    sprintf ("    return names.map(function(nm){return nm.split('%s');}).reduce(function(a,b){return a.concat(b);});", separator),
		    "}", sep="\n"));
    chmRegisterTypeMapper (listtype, itemtype, 'javascript', fn=fn);
}

#' List known axis types.
#'
#' This function returns a list of the axis types for which axis- or matrix- menu entries may be defined.
#'
#' @param re Only types with names matching re are returned (default ".*")
#'
#' @return a character vector of axis type names
#'
#' @export
#'
#' @seealso [chmAddAxisType()]
chmListTypes <- function (re=".*") {
    if (typeof (re) != "character") {
        stop (sprintf ("Parameter 're' must have type 'character', not '%s'", typeof(re)));
    }
    if (length (re) != 1) {
        stop (sprintf ("Parameter 're' must have a single value, not %d", length(re)));
    }
    at <- lapply (ngchm.env$axisFunctions, function(x)x@type);
    rt <- lapply (ngchm.env$matrixFunctions, function(x)x@rowtype);
    ct <- lapply (ngchm.env$matrixFunctions, function(x)x@columntype);
    tt <- lapply (ngchm.env$typeMappers, function(x)x@fromtype);
    allt <- c(at, rt, ct, tt, recursive = TRUE);
    allt <- allt[!duplicated(allt)];
    mm <- vapply (allt, function(x)length(grep(re,x)) > 0, TRUE)
    return (allt[mm]);
}

#' Register a Javascript function for use in the NGCHM toolbox.
#'
#' This function registers a Javascript function that can included in
#' the toolbox of an NGCHM.  This function is
#' intended for use by NGCHM system developers.
#'
#' @param tbtype The toolbox type.
#' @param menulabel The base menu label for the toolbox operation.
#' @param jsfn The Javascript function that implements the toolbox operation.
#'
#' @return NULL
#'
#' @export
#'
#' @seealso [chmNewFunction()]
#' @seealso [ngchmAxisFunction-class]
#' @seealso [ngchmMatrixFunction-class]
#'
chmRegisterToolboxFunction <- function (tbtype, menulabel, jsfn) {
    if (typeof (menulabel) != "character") {
        stop (sprintf ("Parameter 'menulabel' must have type 'character', not '%s'", typeof(menulabel)));
    }
    if (length (menulabel) != 1) {
        stop (sprintf ("Parameter 'menulabel' must have a single value, not %d", length(menulabel)));
    }
    if (nchar (menulabel) == 0) {
        stop ("Parameter 'menulabel' cannot be the empty string");
    }
    if (typeof (tbtype) != "character") {
        stop (sprintf ("Parameter 'tbtype' of toolbox function '%s' must have type 'character', not '%s'", menulabel, typeof(tbtype)));
    }
    if (length (tbtype) != 1) {
        stop (sprintf ("Parameter 'tbtype' of toolbox function '%s' must have a single value, not %d", menulabel, length(tbtype)));
    }
    if (nchar (tbtype) == 0) {
        stop (sprintf ("Parameter 'tbtype' of toolbox function '%s' cannot be the empty string", menulabel));
    }
    if (!is(jsfn, "ngchmJS")) {
        stop (sprintf ("a toolbox function must be of class ngchmJS (see chmNewFunction) not '%s'",
	               classstr(jsfn)));
    }
    if (length(jsfn@extraParams) != 1) {
        stop (sprintf ("a toolbox function requires exactly 1 extra parameter (called 'dataset'), not %d",
	               length(jsfn@extraParams)));
    }
    if (jsfn@extraParams != "dataset") {
        stop (sprintf ("a toolbox function requires exactly 1 extra parameter called 'dataset', not '%s'",
	               jsfn@extraParams));
    }
    matches <- NULL;
    if (length(ngchm.env$toolbox) > 0) {
        matches <- which (apply (ngchm.env$toolbox, 1, function(tbf) (tbf$label == menulabel)&&(tbf$type==tbtype)));
    }
    if (length (matches) > 0) {
	ngchm.env$toolbox[[matches,"fn"]] <- jsfn;
    } else {
	ngchm.env$toolbox <- rbind (ngchm.env$toolbox, list(type=tbtype, label=menulabel, fn=jsfn));
    }
    NULL
}

#' Check that the specified layer can be added to the specificed NGCHM.
#'
#' Stops with an error if the specified layer cannot be added to the specified NGCHM.
#'
#' @param chm The CHM to which the new layer is being added.
#' @param layer The new layer is being added.
#'
#' @return NULL
#'
validateNewLayer <- function (chm, layer)
{
    newnames <- dimnames (layer);
    if (length (chm@layers) > 0) {
        # Check new layer is compatible with first layer.
        oldnames <- dimnames (chm@layers[[1]]);
	layer1 <- chm@layers[[1]];
	if (any (vapply(newnames,length,0) != vapply(oldnames,length,0))) {
	    stop (sprintf ('Dimensions of new layer "%s" (%dx%d) for CHM "%s" differ from those of existing layers (%dx%d)',
	                   layer@name, nrow(layer), ncol(layer), chm@name, nrow(layer1), ncol(layer1)));
	}
	nm <- newnames[[1]];
	nm1 <- oldnames[[1]];
	if (!setequal (nm, nm1)) {
	    m <- sprintf ('Row names of new layer "%s" for CHM "%s" differ from those of existing layers',
			   layer@name, chm@name);
	    errs <- namesdifferror ('new layer', nm, 'existing layers', nm1);
	    stop (paste (c (m, errs), collapse="\n"));
	}
	nm <- newnames[[2]];
	nm1 <- oldnames[[2]];
	if (!setequal (nm, nm1)) {
	    m <- sprintf ('Column names of new layer "%s" for CHM "%s" differ from those of existing layers',
			   layer@name, chm@name);
	    errs <- namesdifferror ('new layer', nm, 'existing layers', nm1);
	    stop (paste (c (m, errs), collapse="\n"));
	}
    } else {
        # First layer.  Check names are compatible with class bars, if any.
	layername <- sprintf ('new layer "%s"', layer@name);
	if (length (chm@rowCovariateBars) > 0) {
	    for (ii in 1:length(chm@rowCovariateBars)) {
	        validateCovariateBar (chm, "Row", layername, newnames[[1]], chm@rowCovariateBars[[ii]]);
	    }
	}
	if (length (chm@colCovariateBars) > 0) {
	    for (ii in 1:length(chm@colCovariateBars)) {
	        validateCovariateBar (chm, "Column", layername, newnames[[2]], chm@colCovariateBars[[ii]]);
	    }
	}
        # Check names are compatible with row/column orders, if any.
	if (!is(chm@rowOrder,"function")) {
	    validateAxisOrder (chm, "Row", layername, newnames[[1]], chm@rowOrder);
        }
	if (!is(chm@colOrder,"function")) {
	    validateAxisOrder (chm, "Column", layername, newnames[[2]], chm@colOrder);
        }
    }
}

validateNewCovariateBar <- function (chm, where, bar)
{
    if (length (chm@layers) > 0) {
	layer <- chm@layers[[1]];
	layername <- sprintf ('layer "%s"', layer@name);
	if (where %in% c("row", "both")) {
            labels <- ngchmGetLabelsStr (layer@data, "row");
	    validateCovariateBar (chm, "Row", layername, labels, bar);
	}
	if (where %in% c("column", "both")) {
            labels <- ngchmGetLabelsStr (layer@data, "column");
	    validateCovariateBar (chm, "Column", layername, labels, bar);
	}
    }
};

validateCovariateBar <- function (chm, where, layername, labels, bar)
{
    repo <- ngchmFindRepo (bar@data);
    barData <- ngchmLoadDatasetBlob (repo, bar@data, if(bar@type=='discrete')"" else 0.0)$mat;
    if (length (intersect (labels, rownames(barData))) == 0) {
	m <- sprintf ('%s names of %s for CHM "%s" are completely different from those of covariate bar "%s"',
		       where, layername, chm@name, bar@label);
	errs <- namesdifferror (layername, labels, sprintf ('covariate bar "%s"', bar@label), rownames(barData));
	stop (paste (c (m, errs), collapse="\n"));
    }
};

validateNewAxisOrder <- function (chm, where, order)
{
    if (length (chm@layers) > 0) {
	layer <- chm@layers[[1]];
	layername <- sprintf ('layer "%s"', layer@name);
	if (where %in% c("row", "both")) {
            labels <- ngchmGetLabelsStr (layer@data, "row");
	    validateAxisOrder (chm, "Row", layername, labels, order);
	}
	if (where %in% c("column", "both")) {
            labels <- ngchmGetLabelsStr (layer@data, "column");
	    validateAxisOrder (chm, "Column", layername, labels, order);
	}
    }
};

validateAxisOrder <- function (chm, where, layername, labels, order)
{
    # NULL order allowed.
    if (length(order) == 0) return();
    # Convert other order types in a label vector.
    if (is(order, "dendrogram")) {
        order <- labels(order);
    } else if (is(order,"hclust")) {
        order <- labels(as.dendrogram(order));
    } else {
        stopifnot (mode(order) == "character")
    }

    # Order must be a permutation of labels.
    # so lengths must match
    if (length(labels) != length(order)) {
	m <- sprintf ('Number of %s names of %s for CHM "%s" (%d) differs from number of labels in order (%d)',
		       where, layername, chm@name, length(labels), length(order));
	errs <- namesdifferror (layername, labels, 'order', order);
	stop (paste (c (m, errs), collapse="\n"));
    }
    # and intersection must have the same length as well.
    if (length (intersect (labels, order)) != length(labels)) {
	m <- sprintf ('%s names of %s for CHM "%s" differ from those of the specified order',
		       where, layername, chm@name);
	errs <- namesdifferror (layername, labels, 'order', order);
	stop (paste (c (m, errs), collapse="\n"));
    }
};

namesdifferror <- function (desc1, names1, desc2, names2)
{
    msg <- c();
    #msg <- c(msg,sprintf ("Names in %s: %s", desc1, shortnameslist (names1)));
    #msg <- c(msg,sprintf ("Names in %s: %s", desc2, shortnameslist (names2)));
    only1 <- setdiff (names1, names2);
    if (length(only1) == 0) {
	msg <- c(msg,sprintf ("No names in %s but not %s", desc1, desc2));
    } else {
	msg <- c(msg,sprintf ("%d name(s) in %s but not %s: %s", length(only1), desc1, desc2, shortnameslist (only1)));
    }
    only2 <- setdiff (names2, names1);
    if (length(only2) == 0) {
	msg <- c(msg,sprintf ("No names in %s but not %s", desc2, desc1));
    } else {
	msg <- c(msg,sprintf ("%d name(s) in %s but not %s: %s", length(only2), desc2, desc1, shortnameslist (only2)));
    }
    msg
}

shortnameslist <- function (names, maxnames=5)
{
    if (length(names) > maxnames) {
	qnames <- c(sprintf ('"%s"', names[1:maxnames]),'...');
    } else {
	qnames <- sprintf ('"%s"', names);
    }
    paste (qnames, collapse=", ")
}

#' Create a new Dialog for a NGCHM.
#'
#' This function creates a new Dialog suitable for adding to a Next Generation Clustered Heat Map.
#'
#' @param id The html id for the dialog.
#' @param title The dialog title / menu entry name.
#' @param fn The javascript function for customizing the dialog's contents.
#'
#' @return An object of class ngchmDialog
#'
#' @export
#'
#' @seealso [chmAdd()]
#' @seealso [chmAddDialog()]
#'
chmNewDialog <- function (id, title, fn) {
    if (is(fn, "character")) {
        fn <- chmGetFunction (fn);
    }
    dialog <- new (Class="ngchmDialog", id=id, title=title, fn=fn);
    dialog
};

#' Create a new Axis for adding to an NG-CHM.
#'
#' This function creates a new Axis for adding to a Next Generation Clustered Heat Map.  You can specify any
#' axis name here, but chmAdd only accepts row, column, and both.
#'
#' @param axis The name of the axis
#' @param ... Objects to add to the axis
#'
#' @return An object of class ngchmAxis
#'
#' @export
#'
#' @seealso [chmAdd()]
chmAxis <- function (axis, ...) {
    new ("ngchmAxis", axis=axis, objects=list(...))
}

#' Create a new AxisType for adding to an ngchmAxis.
#'
#' This function creates a new AxisType for adding to an ngchmAxis.
#'
#' @param typename The name of the axis type
#' @param func A javascript function (optional) for obtaining a value of that type from the axis
#'
#' @return An object of class ngchmAxisType
#'
#' @export
#'
#' @seealso [chmAxis()]
chmAxisType <- function (typename, func) {
    stopifnot (typeof(typename) == "character" && length(typename)==1);
    if (missing(func)) {
        func <- chmGetFunction ("getLabelValue");
    } else if (typeof(func) == "character" && length(func)==1) {
        func <- chmGetFunction (func);
    } else {
        stopifnot (is(func,"ngchmJS"))
    }
    new (Class="ngchmAxisType", where="", type=typename, func=func)
};

#' Return the names of the NGCHM servers defined to date in this session.
#'
#' @export

chmListServers <- function () {
    vapply (ngchm.env$servers, function (svr) svr$name, "")
}

#' Get a HTTR handle for the server's view/WS URL
#'
#' This function returns a 'handle' suitable for use with the server's view/WS URL
#'
#' @param server An object of class ngchmServer
#'
#' @return An HTTR handle
#'
#' @import httr
#' @import jsonlite
#'
#' @export
#'
ngchmGetHandleHTTR <- function (server) {
    ws <- if (is (server, "ngchmServer")) server@serverURL else server;
    if (!exists (ws, ngchm.env$handledb)) {
	assign (ws, httr::handle(ws), ngchm.env$handledb);
    }
    get (ws, ngchm.env$handledb)
}

#' Return response content interpreted as JSON
#'
#' @param httrResponse The httr response object
#'
#' @return The response parsed as JSON and returned as an R object
#'
#' @export
ngchmResponseJSON <- function (httrResponse) {
    stopifnot (httr::status_code(httrResponse) >= 200 && httr::status_code(httrResponse) < 300);
    jsonlite::fromJSON(content(httrResponse,'text',encoding='UTF-8'), simplifyVector = FALSE)
}

getServerVersion <- function (server) {
    url <- if (is (server, "ngchmServer")) server@serverURL else server;
    ws <- sprintf("%s/gdacws/servermetadata", url);
    res <- httr::GET (ws, handle=ngchmGetHandleHTTR (server));
    as.numeric(ngchmResponseJSON(res)$Build_Number)
}

testExternalProgram <- function (program) {
    res <- NULL;
    suppressWarnings(try ({res <- system2(program, NULL, stdout=TRUE, stderr=TRUE);}, silent=TRUE));
    if (is.null(res)) warning (sprintf("Unable to execute external program '%s'. Some functionality not available.", program));
    !is.null(res)
}

testJava <- function (jarfile) {
    res <- NULL;
    suppressWarnings(try ({res <- system2("java", c("-jar", jarfile), stdout=TRUE, stderr=TRUE);}, silent=TRUE));
    if (is.null(res)) stop ("Unable to execute Java");
    if (length(res) == 0 || res[length(res)] != "WRONG_NUMBER_OF_PARAMETERS_ERROR") stop ("Bad Java version");
}

getBuilderJar <- function (server) {
    if (is.null (server@jarFile)) {
        vvv <- sprintf ("version%d", getServerVersion (server));
        if (!exists (vvv, ngchm.env$jarCache)) {
	    ws <- sprintf("%s/resources/heatmappipeline.jar", server@serverURL);
            res <- httr::GET (ws, handle=ngchmGetHandleHTTR (server));
	    tmpJarFile <- utempfile ("hmt", fileext=".jar");
	    writeBin (res$content, tmpJarFile);
	    testJava (tmpJarFile);
	    assign (vvv, tmpJarFile, ngchm.env$jarCache);
	}
        return (get (vvv, ngchm.env$jarCache))
    }
    if (!exists (server@jarFile, ngchm.env$jarCache)) {
        # Load server@jarFile into jarCache
	if (length(grep("^scp://", server@jarFile)) > 0) {
	    tmpJarFile <- utempfile ("hmt", fileext=".jar");
	    parts <- URLparts (server@jarFile);
	    if (parts[3] == "") {
		systemCheck (sprintf ("scp %s:%s %s",
			      shQuote (parts[2]), shQuote(parts[4]), tmpJarFile));
	    } else {
		systemCheck (sprintf ("scp -P %s %s:%s %s",
			      shQuote(parts[3]), shQuote (parts[2]), shQuote(parts[4]), tmpJarFile));
	    }
	    ngchm.env$jarCache[[server@jarFile]] <- tmpJarFile;
	}
	else if (length(grep("^http://", server@jarFile)) > 0) {
	    tmpJarFile <- utempfile ("hmt", fileext=".jar");
            download.file (server@jarFile, tmpJarFile);
	    ngchm.env$jarCache[[server@jarFile]] <- tmpJarFile;
	}
	else if (length(grep("^https://", server@jarFile)) > 0) {
	    tmpJarFile <- utempfile ("hmt", fileext=".jar");
            download.file (server@jarFile, tmpJarFile);
	    ngchm.env$jarCache[[server@jarFile]] <- tmpJarFile;
	}
	else if (length(grep("^file://", server@jarFile)) > 0) {
	    ngchm.env$jarCache[[server@jarFile]] <- substring (server@jarFile, 8);
	} else {
	    stop (sprintf ("Unknown retrieval method for jarFile '%s' for NGCHM server '%s'", server@jarFile, server@name));
	}
	testJava (ngchm.env$jarCache[[server@jarFile]]);
    }
    ngchm.env$jarCache[[server@jarFile]]
}

#' Create an ngchmServer object from a specification.
#'
#' Create an ngchmServer object called 'serverName' from the specification 'serverSpec' (see details).
#' serverOptions override those in the specification files option by option.
#' The new ngchmServer object is returned and registered so that it can be
#' referenced by name, including retrieval using chmServer.
#'
#' @param serverName The name of the new server object.
#' @param serverSpec The specification for the server (defaults to servername).
#' @param serverOptions A named list of server options.
#'
#' @return The created (and registered) ngchmServer object.
#' @export
#'
#' @details serverSpec can be any of:
#' \itemize{
#'   \item{A configuration directory path. }{The specification will be read from a file 'config.txt' in that directory.}
#'   \item{An NGCHM server URL (ending in '/chm' or '/Viewer' for instance). }{A minimal specification will be inferred.
#'         Known methods for uploading NGCHMs to the server will be autoprobed unless specified manually.}
#'   \item{A URL referencing a configuration file (must end in '/config.txt'). }{The specification will be read from the
#'         specified URL.}
#' }
#' serverOptions can include both protocol-specific options and the following generic options:
#' \itemize{
#'  \item{'serverURL'. }{The URL for the NGCHM server.}
#'  \item{'serverProtocol'. }{The protocol to be used for uploading etc. NGCHMs to the server.}
#'  \item{'jarFile'. }{The jarFile used to build NGCHMs.}
#'  \item{'traceLevel'. }{The amount of trace to output. Defaults to "PROGRESS".}
#' }
#'
#' @seealso [chmServer()]
#' @seealso [ngchmServer-class]
#' @seealso [ngchmGetServerProtocol()]
#' @seealso [ngchmServerProtocol-class]


chmCreateServer <- function (serverName,
			     serverSpec = NULL,
			     serverOptions = NULL)
{
    defaultOptions <- list (traceLevel="PROGRESS", serverProtocol="manual", jarFile=NULL);
    if (is.null(serverSpec)) serverSpec <- serverName;

    cfg <- new.env();

    if (substr (serverSpec, 1, 2) == "//") {
        serverSpec <- paste ("http:", serverSpec, sep="");
    }

    hasProto <- grepl ("^[^/]*:", serverSpec);
    if (!hasProto) {
	# No protocol, so try to interpret as a server configuration directory
	if (file.exists (file.path (serverSpec, 'config.txt'))) {
	    readConfigFile (cfg, file.path (serverSpec, "config.txt"), '=');
	} else {
	    stop (sprintf ("'%s' is not a server configuration directory", serverSpec));
	}
    } else {
        proto <- sub (":.*", "", serverSpec);
	if (!(proto %in% c('http', 'https', 'scp', 'file'))) {
	    stop (sprintf ("Unknown protocol in serverSpec '%s'", serverSpec));
	}
	if (proto == 'file') {
	    serverSpec <- sub("file:","",serverSpec);
	    if (file.exists (file.path (serverSpec, 'config.txt'))) {
		readConfigFile (cfg, file.path (serverSpec, "config.txt"), '=');
	    } else {
		stop (sprintf ("'%s' is not a server configuration directory", serverSpec));
	    }
	}
	else if (proto %in% c('http', 'https')) {
	    if (substring(serverSpec,nchar(serverSpec)-10) == "/config.txt") {
		readConfigFile (cfg, serverSpec, '=');
	    } else {
	        # Assume a URL that refers to an NGCHM server.
		cfg$serverURL <- serverSpec;
		# If protocol not specified, probe to see if API available.
                if (!'serverProtocol' %in% names(serverOptions)) {
                    foundAPI <- FALSE;
                    # try manager API
		    ws <- sprintf("%s/manager/rest/chmservers", serverSpec);
		    res <- httr::GET (ws, handle=ngchmGetHandleHTTR (serverSpec));
		    if (res$status_code >= 200 && res$status_code < 300) {
			content <- try(ngchmResponseJSON(res), silent=TRUE);
			if ((!is(content, 'try-error')) && (length(content) > 0)) {
			    cfg$serverProtocol <- 'manager';
			    cfg$deployServer <- sprintf ("%s/manager/rest", serverSpec);
			    cfg$serviceName <- names(content)[1];
                            foundAPI <- TRUE;
			}
		    }
                    if (!foundAPI) {
                        # try shaidy API
			ws <- sprintf("%s/api/", serverSpec);
			res <- httr::GET (ws, handle=ngchmGetHandleHTTR (serverSpec));
			if (res$status_code >= 200 && res$status_code < 300) {
			    content <- try(ngchmResponseJSON(res), silent=TRUE);
			    if ((!is(content, 'try-error')) && (length(content) > 0)) {
				cfg$serverProtocol <- 'shaidy';
				cfg$basePath <- sprintf ("%s/api", serverSpec);
				cfg$viewServer <- sprintf ("%s/NGCHM", serverSpec);
				cfg$accessMethod <- 'api';
                                foundAPI <- TRUE;
			    }
			}
                    }
		}
	    }
	}
	else if (proto == "scp") {
	    defaultOptions$serverProtocol <- "mds2";
	    spec <- sub ("^[^/]*:", "", serverSpec);
	    if (substr (spec, 1, 2) != "//") stop (sprintf ("Malformed server spec '%s'", serverSpec));
	    spec <- substring (spec, 3);
	    cfgServer <- sub ("/.*", "", spec);
	    cfgDir <- sub ("^[^/]*", "", spec);
	    if (nchar (defaultOptions$deployServer) == 0) stop (sprintf ("Malformed server spec '%s'", serverSpec));
	    if (nchar (defaultOptions$deployDir) == 0) stop (sprintf ("Malformed server spec '%s'", serverSpec));
	    defaultOptions$deployServer <- cfgServer;
	    defaultOptions$deployDir <- cfgDir;

	    cfgFile <- utempfile ("cfg", fileext=".txt");
	    rcfg <- new.env();
	    if (system2 ("scp", args=c(sprintf ("%s:%s/%s", cfgServer, cfgDir, "config.txt"), cfgFile)) == 0) {
		readConfigFile (rcfg, cfgFile, '=');
	    }
	    rcfg <- as.list (rcfg);
	    if (!("jarFile" %in% names(serverOptions))) {
		# No user specified jarFile, so let's see what the remote config says.
		if (length (rcfg$jarFile) == 0) {
		    # No jarFile specified in remote config, so peek to see if there's a remote jarFile in .mds subdirectory.
		    jarFile <- paste (cfgDir, ".mds", "heatmappipeline.jar", sep="");
		    found <- system2 ("ssh", args=c(cfgServer, sprintf ("[ -r %s ]", jarFile)));
		    if (found == 0) {
			# Shell command above succeeded.
			rcfg$jarFile <- paste ("scp://", cfgServer, jarFile, sep="");
		    }
		}
		else if (substr (rcfg$jarFile, 1, 7) == "file://") {
		    # Remote specified jarFile using file://, so convert to an scp://
		    jarFile <- substr (rcfg$jarFile, 8, nchar (rcfg$jarFile));
		    if (substr (jarFile, 1, 1) != '/') {
			jarFile <- paste ('', cfgDir, jarFile, sep="/");
		    }
		    found <- system2 ("ssh", args=c(cfgServer, sprintf ("[ -r %s ]", jarFile)));
		    if (found == 0) {
			rcfg$jarFile <- paste ("scp://", cfgServer, jarFile, sep="");
		    } else {
			stop (sprintf ("Unable to access remote mds2 jarfile scp://%s%s", cfgServer, jarFile));
		    }
		}
		else {
		    # We assume network based addresses will work equally well from the current machine.
		}
	    }
	    for (field in names(rcfg)) {
		cfg[[field]] <- rcfg[[field]];
	    }
	}
    }
    fileOptions <- as.list (cfg);

    if ("jarFile" %in% names(serverOptions)) {
	# the user explicitly specified the jarFile to use.  Use it or fail.
	if (file.exists (serverOptions$jarFile)) {
	    serverOptions$jarFile <- paste ("file://", serverOptions$jarFile, sep="");
	}
	else {
	    stop (sprintf ("Specified jarFile '%s' does not exist", serverOptions$jarFile));
	}
    }

    classFields <- c('traceLevel', 'serverProtocol', 'deployServer', 'jarFile', 'serverURL', 'viewServer')

    # defaultOptions, overridden by file options, overridden by explicit serverOptions.
    cfg <- defaultOptions;
    for (field in names(fileOptions)) {
	cfg[[field]] <- fileOptions[[field]];
    }
    for (field in names(serverOptions)) {
	cfg[[field]] <- serverOptions[[field]];
    }

    stopifnot ('serverProtocol' %in% names(cfg));
    protocol <- ngchmGetServerProtocol (cfg$serverProtocol);
    protoOpts <- cfg[setdiff(names(cfg),classFields)];
    ngchmProtoParamCheck (protoOpts, protocol@requiredParams, protocol@optionalParams);
    protocol@paramValidator (protoOpts);

    ngchmRegisterServer("base", new(Class="ngchmServer",
			   name = serverName,
			   serverURL = cfg$serverURL,
			   serverProtocol = protocol,
			   traceLevel = cfg$traceLevel,
			   jarFile = cfg$jarFile,
			   deployServer = cfg$deployServer,
                           viewServer = cfg$viewServer,
			   protoOpts = protoOpts));
}

#' Create an ngchmServer object for a managed NG-CHM server
#'
#' Create an ngchmServer object called 'serverName' (see details).
#' The new ngchmServer object is returned and registered so that it can be
#' referenced by name, including retrieval using chmServer.
#' This library will communicate with the NG-CHM using the private address.
#' Returned URLs for viewing NG-CHMs will use the public address.
#'
#' @param serverName The name of the new server object.
#' @param privateAddr Private IP name/address of the server.
#' @param publicAddr Public IP name/address of the server.
#' @param chmPort Port on which the chm viewer is listening.
#' @param managerPort Port on which the chm manager is listening.
#' @param serviceName Name of the chmManager service
#' @param ... Additional options passed to chmCreateServer
#'
#' @return The created (and registered) ngchmServer object.
#' @export
#'
#' @seealso [chmServer()]
#' @seealso [chmCreateServer()]

chmCreateManagedServer <- function (serverName, privateAddr, publicAddr=NULL, chmPort=80, managerPort=18080, serviceName="default", ...) {
    if (is.null(publicAddr)) publicAddr <- privateAddr;
    chmCreateServer (serverName,
                     sprintf ("http://%s:%d/chm", privateAddr, chmPort),
                     list (serverProtocol="manager",
                           deployServer=sprintf("http://%s:%d/chm/manager/rest", privateAddr, managerPort),
                           serviceName=serviceName,
                           viewServer=sprintf("http://%s:%d/chm", publicAddr, chmPort),
                           ...))
}

#' Check that all required parameters are specified, and all specified parameters are either required or optional.
#'
#' @param params A list of named parameters.
#' @param required A character vector of required parameter names
#' @param optional A character vector of optional parameter names
#'
#' @export
#'
ngchmProtoParamCheck <- function (params, required, optional) {
    nm <- names(params);
    need <- setdiff (required, nm);
    if (length(need) != 0) stop (sprintf ("Required protocol parameter(s) %s not specified", paste(need,collapse=',')));
    unknown <- setdiff (nm, c(required,optional));
    if (length(unknown) != 0) stop (sprintf ("Unknown protocol parameter(s) %s specified", paste(unknown,collapse=',')));
}

readConfigFile <- function (cfg, filename, sep=':') {
    lines <- NULL;
    try (suppressWarnings(lines <- readLines (filename)), silent=TRUE);
    if (length (lines) > 0) {
	for (line in strsplit (lines, sep)) {
	    if (length(line) != 2) stop (sprintf ('Malformed configuration line "%s" in %s: should be option%svalue', paste(line,sep=sep), filename, sep));
	    cfg[[line[1]]] <- line[2];
	}
    }
}

getExtraParams <- function (props) {
    # Collect extraparams.
    pat <- "^!extraparam:";
    idx <- which (vapply (props, function(x)regexpr(pat,x@label)!=-1, TRUE));
    params <- new.env();
    for (pp in props[idx]) {
        match <- regexpr (pat, pp@label);
	paramname <- substring (pp@label, attr(match,'match.length')+1);
	params[[paramname]] <- pp@value;
    }
    cat ('Found extra params ', paste(ls(params),collapse=" "), "\n")
    params
}

bindExtraParams <- function (funcs, params, props) {
    hasep <- vapply (funcs, function(fn)length(fn@func@extraParams)>0, TRUE);
    epfuncs <- funcs[which(hasep)];
    funcs <- funcs[which(!hasep)];
    for (fn in epfuncs) {
        extra <- fn@func@extraParams;
	cat ('Function ', fn@func@name, ' requires extra params ', paste(extra,collapse=" "), "\n");
	if (all (vapply (extra, function(p) exists(p, params), TRUE))) {
	    vals <- vapply (extra, function(p)params[[p]], "");
	    names(vals) <- extra;
	    tmpname <- paste(c('f',sample(c(letters,toupper(letters),as.character(0:9)),15,replace=TRUE)), collapse="");
	    chmBindFunction (tmpname, fn@func@name, as.list(vals));
	    if (is(fn, "ngchmAxisFunction"))
		newfn <- new(class(fn), type=fn@type, label=fn@label, func=chmGetFunction(tmpname))
	    else if (is(fn, "ngchmMatrixFunction"))
		newfn <- new(class(fn), rowtype=fn@rowtype, coltype=fn@coltype, label=fn@label, func=chmGetFunction(tmpname))
	    else
	        stop (sprintf ("Unknown function class '%s'", classstr(fn)));
	    funcs <- append (funcs, newfn);
	}
    }
    funcs
}

chmAddAutoMenuItems <- function (chm) {
    params <- getExtraParams (chm@properties);

    # Add row menu items for types we know about.
    genSpecFeedback (10, "adding row menu items");
    rowtypes <- getAllAxisTypes (chm, "row");
    rowTypeFnsReqd <- rep(FALSE,length(rowtypes));
    rowfns <- getAllAxisTypeFunctions (chm@rowTypeFunctions, rowtypes$types);
    rowfns <- bindExtraParams (rowfns, params, chm@properties);
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
    colfns <- bindExtraParams (colfns, params, chm@properties);
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
    matfns <- bindExtraParams (matfns, params, chm@properties);
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
	     } else if (is (fns[[ii]], "ngchmAxisType")) {
		 chm <- chmAddMenuItem (chm, "nowhere", "unused", fns[[ii]]@func);
	     } else if (is (fns[[ii]], "ngchmAxisFunction")) {
		 chm <- chmAddMenuItem (chm, "nowhere", "unused", fns[[ii]]@func);
	     } else if (is (fns[[ii]], "ngchmMatrixFunction")) {
		 chm <- chmAddMenuItem (chm, "nowhere", "unused", fns[[ii]]@func);
	     } else if (fns[[ii]]@op != "javascript"){
		cat ("chmMake: axis/mat fn op is not javascript\n", file=stderr());
	     } else {
		 chm <- chmAddMenuItem (chm, "nowhere", "unused", fns[[ii]]@params$func);
	     }
	 }

    fns <- unique (append (rowtypes$builders[rowTypeFnsReqd], coltypes$builders[colTypeFnsReqd]));
    if (length(fns) > 0)
         for (ii in 1:length(fns))
	     if (length(fns[[ii]]) == 0) {
		cat ("chmMake: builders fn entry is NULL\n", file=stderr());
	     } else if (is (fns[[ii]], "ngchmAxisType")) {
		 chm <- chmAddMenuItem (chm, "nowhere", "unused", fns[[ii]]@func);
	     } else if (is (fns[[ii]], "ngchmAxisFunction")) {
		 chm <- chmAddMenuItem (chm, "nowhere", "unused", fns[[ii]]@func);
	     } else if (fns[[ii]]@op != "javascript"){
		cat ("chmMake: builders fn op is not javascript\n", file=stderr());
	     } else {
		 chm <- chmAddMenuItem (chm, "nowhere", "unused", fns[[ii]]@params$func);
	     }

    chmUU (chm)
}

#' Output Javascript code required to customize an NGCHM.
#'
#' This function outputs the Javascript required to customize an NGCHM.
#'
#' @param chm An NGCHM
#' @param filename The file to which the customization code will be written.
#'
#' @export
#'
chmWriteCustomJS <- function (chm, filename) {
    chm <- chmFixVersion (chm);
    if (length (chm@datasets) > 0) {
	#genSpecFeedback (8, "adding toolbox(es)");
	chm <- addToolBoxes (chm);
    }
    chm <- chmAddAutoMenuItems (chm);
    writeCustomJS (chm, filename);
}

#' Get the file path to the specified overview file.
#'
#' This function returns the file path to the specified overview image of the CHM.  The CHM must be
#' made before the file can be accessed.
#' If idx is specified, format if given must equal that of the overview image, and the path to that overview image is returned.
#' If idx is not specified, the file path to the first overview of the given format (default 'png') is returned.
#'
#' @param chm An NGCHM.
#' @param format The format of overview image desired (defaults to 'png' if idx is not specified).
#' @param idx The index of the overview image desired (defaults to first image of the specified format).
#'
#' @export
chmGetOverview <- function (chm, format=NULL, idx=NULL) {
    chm <- chmFixVersion (chm);
    if (is.null(idx)) {
	if (is.null (format)) format <- 'png';
	idx <- which(vapply (chm@overviews, function(ov)ov@format, '')==format);
	if (is.null(idx)) stop (sprintf ('CHM "%s" has no overview with format "%s"', chm@name, format));
	idx <- idx[1];
    } else {
	if ((idx < 1) || (idx > length(chm@overviews)))
	    stop (sprintf ('Invalid index (%d) for overview of CHM "%s"', idx, chm@name));
        if (is.null (format)) {
	    format <- chm@overviews[[idx]]@format;
	}
	else if (format != chm@overviews[[idx]]@format) {
	    stop (sprintf ('Overview #%d of CHM "%s" does not have format "%s"', idx, chm@name, format));
	}
    }
    file.path(chm@outDir, chm@name, 'overview', sprintf('overview%d.%s', idx, format))
}

#' @import digest
getuuid <- function(prev="") {
    digest::digest(paste(c(Sys.info(),Sys.time(),rnorm(1),prev,collapse=""),recursive=TRUE),algo="sha256")
}

chmFixCovariateVersion <- function (covariate) {
    if (!is(covariate@label.series,'shaid')) {
        values <- covariate@label.series;
        stopifnot (is (values, 'character'));
        values <- values[order(names(values))];
        mat <- matrix (values, ncol=1, dimnames=list(names(values),'Value'));
        covariate@label.series <- ngchmSaveAsDatasetBlob (ngchm.env$tmpShaidy, 'tsv', mat);
    }
    covariate
}

chmFixDatasetVersion <- function (dataset) {
    dataset@row.covariates <- lapply (dataset@row.covariates, chmFixCovariateVersion);
    dataset@column.covariates <- lapply (dataset@column.covariates, chmFixCovariateVersion);
    dataset
}

chmFixVersion <- function (chm) {
    if (chm@version < 2) {
        warning ("Upgrading chm ", chm@name, " from version ", chm@version, " to version 2");
        v2 <- new ("ngchmVersion2");
        for (name in slotNames("ngchmVersion1")) {
	    if (!name %in% c("version", "rowClassbars", "colClassbars")) {
	        slot (v2, name) <- slot (chm, name);
	    }
        }
	v2@rowCovariateBars <- chm@rowClassbars;
	v2@colCovariateBars <- chm@colClassbars;
        for (ii in 1:length(chm@layers)) {
	    if (is(chm@layers[[ii]]@data,"matrix")) {
                v2@layers[[ii]]@data <- ngchmSaveAsDatasetBlob (ngchm.env$tmpShaidy, 'tsv', v2@layers[[ii]]@data);
	    }
        }
	chm <- v2;
    }
    if (chm@version == 2) {
        if (length(chm@layers) > 0) {
    	    # Early version2 did not have summarizationMethod slot
            for (ii in 1:length(chm@layers)) {
    	    if (!.hasSlot(chm@layers[[ii]],"summarizationMethod")) {
                    chm@layers[[ii]]@summarizationMethod <- 'average';
    	    }
            }
        }
        if (length(chm@templates) > 0) {
    	    # Early version2 did not have dest.blob slot
            for (ii in 1:length(chm@templates)) {
    	    if (!.hasSlot(chm@templates[[ii]],"dest.blob")) {
                    t <- chm@templates[[ii]];
                    # FIXME: will not work if source.path is a file that is either no longer available or different
                    # It might be possible to find original template where CHM came from???
                    chm@templates[[ii]]@dest.blob <-
                        ngchmSaveTemplateAsBlob(ngchm.env$tmpShaidy, t@source.path, t@dest.path, t@substitutions);
    	    }
            }
        }
        chm@datasets <- lapply (chm@datasets, chmFixDatasetVersion);
    }
    if (length(chm@rowOrder)>0 && is(chm@rowOrder,"function")) {
        if (!identical(chm@rowOrder,chmDefaultRowOrder) &&
            !identical(chm@rowOrder,chmOriginalRowOrder) &&
            !identical(chm@rowOrder,chmRandomRowOrder)) {
            if (chm@rowOrderMethod=="Random") {
                chm@rowOrder <- chmRandomRowOrder;
            } else if (chm@rowOrderMethod=="Original") {
                chm@rowOrder <- chmOriginalRowOrder;
            } else {
                chm@rowOrder <- chmDefaultRowOrder;
                chm@rowOrderMethod <- "Hierarchical";
            }
        }
    }
    if (length(chm@colOrder)>0 && is(chm@colOrder,"function")) {
        if (!identical(chm@colOrder,chmDefaultColOrder) &&
            !identical(chm@colOrder,chmOriginalColOrder) &&
            !identical(chm@colOrder,chmRandomColOrder)) {
            if (chm@colOrderMethod=="Random") {
                chm@colOrder <- chmRandomColOrder;
            } else if (chm@colOrderMethod=="Original") {
                chm@colOrder <- chmOriginalColOrder;
            } else {
                chm@colOrder <- chmDefaultColOrder;
                chm@colOrderMethod <- "Hierarchical";
            }
        }
    }
    chm
}

chmUU <- function (chm) {
    chm@uuid <- getuuid(chm@uuid)
    chm
}

#' @import digest
gitSha <- function (data) {
    stopifnot (typeof(data) %in% c("character","raw"));
    if (typeof(data) == "character") {
        data <- charToRaw (paste (data, collapse=''));
    }
    head <- sprintf ("blob %d", length(data));
    digest::digest (c(charToRaw(head),raw(1),data),"sha1",serialize=FALSE)
}

utempfile <- function (...) {
    # On Windows, file.path separates files by / , tempfile (and tempdir) separate them by backslashes
    # We need consistency, so convert to canonical separator / here, rather than complicate all other uses
    filename <- tempfile (...);
    if (Sys.info()[['sysname']] == "Windows")  {
        filename <- gsub ("\\\\", "/", filename);
    }
    filename
}

#' Browse the NGCHMs on the specified server in the viewer.
#'
#' Opens the NG-CHM browser page in the viewer.
#'
#' @param server The server to browse. Defaults to option "NGCHM.Server" or the first server.
#' @param viewer The viewer to use. Defaults to option "viewer" or browseURL.
#' @export
#'
#' @seealso [utils::browseURL()]
chmBrowse <- function (server=NULL, viewer=NULL) {
    if (is.null(server)) server <- getOption("NGCHM.Server", chmListServers()[1]);
    if (!is(server,"ngchmServer")) server <- chmServer(server);
    if (is.null(viewer)) viewer <- getOption("viewer", browseURL);
    viewer (server@viewServer)
}

#' Manage the NGCHMs on the specified server in the viewer.
#'
#' Opens the NG-CHM manager page in the viewer.
#'
#' @param server The server to browse. Defaults to option "NGCHM.Server" or the first server.
#'        The server must be managed.
#' @param viewer The viewer to use. Defaults to option "viewer" or browseURL.
#' @export
#'
#' @seealso [utils::browseURL()]
chmManager <- function (server=NULL, viewer=NULL) {
    if (is.null(server)) server <- getOption("NGCHM.Server", chmListServers()[1]);
    if (!is(server,"ngchmServer")) server <- chmServer(server);
    stopifnot (server@serverProtocol@protocolName == "manager");
    if (is.null(viewer)) viewer <- getOption("viewer", browseURL);
    viewer (paste(server@viewServer, "manager", sep="/"))
}

#' Open the NG-CHM on the specified server in the viewer.
#'
#' @param x The NGCHM to view.
#' @param server The server containing the NG-CHM.  Defaults to option "NGCHM.Server" or the first server.
#' @param viewer The viewer to use. Defaults to option "viewer" or browseURL.
#' @param ... Ignored.
#' @export
plot.ngchmVersion2 <- function(x, server=NULL, viewer=NULL, ...) {
    if (is.null(server)) server <- getOption("NGCHM.Server", chmListServers()[1]);
    if (!is(server,"ngchmServer")) server <- chmServer(server);
    if (is.null(viewer)) viewer <- getOption("viewer", browseURL);
    viewer(chmGetURL(x,server=server))
}

#' Write a matrix as a binary viewer tile
#'
#' Write the matrix as a vector of 32-bit little-endian floats to the specified file.
#'
#' @param mat The matrix to output
#' @param filename The name of the file to write the tile to
writeTile <- function (mat, filename) {
    vec <- t (mat);
    dim(vec) <- NULL;
    writeBin (vec, filename, size=4, endian="little");
}

#' Read a binary viewer tile as a matrix
#'
#' Read a vector of little-endian 32-bit floats from the specified file and return
#' as a matrix with nrow rows and ncol columns.
#'
#' @param filename The name of the file to read the tile from
#' @param nrow Number of rows in the tile
#' @param ncol Number of columns in the tile
readTile <- function (filename, nrow, ncol) {
    vec <- readBin (filename, "double", nrow*ncol, size=4, endian="little");
    return (matrix (vec, nrow=nrow, byrow=TRUE));
}

#' Export a standalone NGCHM to a file.
#'
#' Create a standalone viewer for the NGCHM in the specified file.
#'
#' @export
#' @rdname chmExportToFile-method
#'
#' @param chm The NGCHM to export
#' @param filename The file in which to save the rendered NGCHM
#' @param overwrite Overwrite file iff true (default false)
#' @param shaidyMapGen Path to shaidyMapGen jar file (default to value of environment variable SHAIDYMAPGEN)
#' @param shaidyMapGenJava Path to java executable with which to run shaidyMapGen (default to value of environment variable SHAIDYMAPGENJAVA or java)
#' @param shaidyMapGenArgs Additional arguments to pass to java when running shaidyMapGen (default to value of environment variable SHAIDYMAPGENARGS)
#'
#' @return the rendered NGCHM
chmExportToFile <- function(chm,filename,overwrite=FALSE,shaidyMapGen, shaidyMapGenJava, shaidyMapGenArgs) {
    if( !overwrite && file.exists(filename) ) stop ("'filename' already exists");
    if (missing(shaidyMapGen)) shaidyMapGen <- Sys.getenv("SHAIDYMAPGEN");
    if (missing(shaidyMapGenJava)) shaidyMapGenJava <- Sys.getenv("SHAIDYMAPGENJAVA");
    if (shaidyMapGenJava == "") shaidyMapGenJava <- "java";
    if (missing(shaidyMapGenArgs)) shaidyMapGenArgs <- strsplit(Sys.getenv("SHAIDYMAPGENARGS"),",")[[1]];
    if (shaidyMapGen == "") stop ("shaidyMapGen not specified or set in environment");

    chm@format <- "shaidy";
    chm <- chmAddProperty (chm, "chm.info.build.time", format(Sys.time(), "%F %H:%M:%S"));
    chm <- chmMake (chm);

    shaidyRepo <- ngchm.env$tmpShaidy;
    shaid <- shaidyGetShaid (chm);
    status <- system2(shaidyMapGenJava, c(shaidyMapGenArgs, "-jar", shaidyMapGen, shaidyRepo$basepath, shaid@value, shaid@value, "NO_PDF"));
    if (status != 0) stop("export to ngchm failed");
    if (!file.copy (shaidyRepo$blob.path ("viewer", shaid@value, chm@name, paste(chm@name,"ngchm",sep=".")), filename, TRUE)) {
        stop("export to ngchm failed");
    }
    chm
}

#' Export a PDF of the NGCHM to a file.
#'
#' Create a PDF of the NGCHM in the specified file.
#'
#' @export
#' @rdname chmExportToPDF-method
#'
#' @param chm The NGCHM to generate the PDF for
#' @param filename The file in which to save the PDF
#' @param overwrite Overwrite file iff true (default false)
#' @param shaidyMapGen Path to shaidyMapGen jar file (default to value of environment variable SHAIDYMAPGEN)
#' @param shaidyMapGenJava Path to java executable with which to run shaidyMapGen (default to value of environment variable SHAIDYMAPGENJAVA or java)
#' @param shaidyMapGenArgs Additional arguments to pass to java when running shaidyMapGen (default to value of environment variable SHAIDYMAPGENARGS)
#'
#' @return filename
chmExportToPDF <- function(chm,filename,overwrite=FALSE,shaidyMapGen, shaidyMapGenJava, shaidyMapGenArgs) {
    if( !overwrite && file.exists(filename) ) stop ("'filename' already exists");
    if (missing(shaidyMapGen)) shaidyMapGen <- Sys.getenv("SHAIDYMAPGEN");
    if (missing(shaidyMapGenJava)) shaidyMapGenJava <- Sys.getenv("SHAIDYMAPGENJAVA");
    if (shaidyMapGenJava == "") shaidyMapGenJava <- "java";
    if (missing(shaidyMapGenArgs)) shaidyMapGenArgs <- strsplit(Sys.getenv("SHAIDYMAPGENARGS"),",")[[1]];

    if (length(chmProperty(chm, "chm.info.build.time"))==0) {
        chm@format <- "shaidy";
        chmProperty (chm, "chm.info.build.time") <- format(Sys.time(), "%F %H:%M:%S");
        chm <- chmMake (chm);
    }

    shaidyRepo <- ngchm.env$tmpShaidy;
    shaid <- shaidyGetShaid (chm);

    pdfpath <- shaidyRepo$blob.path ("viewer", shaid@value, chm@name, paste(chm@name,".pdf",sep=""));
    if (!file.exists(pdfpath)) {
        if (shaidyMapGen == "") stop ("shaidyMapGen required but not specified or set in environment");
        status <- system2(shaidyMapGenJava, c(shaidyMapGenArgs, "-jar", shaidyMapGen, shaidyRepo$basepath, shaid@value, shaid@value));
        if (status != 0 || !file.exists(pdfpath)) stop("export to pdf failed");
    }

    if (!file.copy (pdfpath, filename, TRUE)) {
        stop("export to pdf failed");
    }
    filename
};

#' Export a standalone HTML containing the NGCHM to a file.
#'
#' Create a standalone HTML containing the NGCHM in the specified file.
#'
#' @export
#' @rdname chmExportToHTML-method
#'
#' @param chm The NGCHM to generate the PDF for
#' @param filename The file in which to save the PDF
#' @param overwrite Overwrite file iff true (default false)
#' @param shaidyMapGen Path to shaidyMapGen jar file (default to value of environment variable SHAIDYMAPGEN)
#' @param shaidyMapGenJava Path to java executable with which to run shaidyMapGen (default to value of environment variable SHAIDYMAPGENJAVA or java)
#' @param shaidyMapGenArgs Additional arguments to pass to java when running shaidyMapGen (default to value of environment variable SHAIDYMAPGENARGS)
#' @param ngchmWidgetPath Path to location of ngchm Widget (ngchmWidget-min.js). Defaults to environment variable NGCHMWIDGETPATH.
#'
#' @return filename
chmExportToHTML <- function(chm,filename,overwrite=FALSE,shaidyMapGen, shaidyMapGenJava, shaidyMapGenArgs, ngchmWidgetPath) {
    if( !overwrite && file.exists(filename) ) stop ("'filename' already exists");
    if (missing(shaidyMapGen)) shaidyMapGen <- Sys.getenv("SHAIDYMAPGEN");
    if (missing(shaidyMapGenJava)) shaidyMapGenJava <- Sys.getenv("SHAIDYMAPGENJAVA");
    if (shaidyMapGenJava == "") shaidyMapGenJava <- "java";
    if (missing(shaidyMapGenArgs)) shaidyMapGenArgs <- strsplit(Sys.getenv("SHAIDYMAPGENARGS"),",")[[1]];
    if (missing(ngchmWidgetPath)) {
        stopifnot (Sys.getenv("NGCHMWIDGETPATH") != "");
    } else {
	Sys.setenv(NGCHMWIDGETPATH=ngchmWidgetPath);
    }


    if (length(chmProperty(chm, "chm.info.build.time"))==0) {
        chm@format <- "shaidy";
        chmProperty (chm, "chm.info.build.time") <- format(Sys.time(), "%F %H:%M:%S");
        chm <- chmMake (chm);
    }

    shaidyRepo <- ngchm.env$tmpShaidy;
    shaid <- shaidyGetShaid (chm);

    htmlpath <- shaidyRepo$blob.path ("viewer", shaid@value, chm@name, paste(chm@name,".html",sep=""));
    if (!file.exists(htmlpath)) {
        if (shaidyMapGen == "") stop ("shaidyMapGen required but not specified or set in environment");
        status <- system2(shaidyMapGenJava, c(shaidyMapGenArgs, "-jar", shaidyMapGen, shaidyRepo$basepath, shaid@value, shaid@value, "NO_PDF", "-HTML"));
        if (status != 0 || !file.exists(htmlpath)) stop("export to html failed");
    }

    if (!file.copy (htmlpath, filename, TRUE)) {
        stop("export to html failed");
    }
    filename
};

writeBinLines <- function(text, con) {
    openit <- is.character(con);
    if (openit) con <- file(con, "wb");
    writeLines (text, con, "\n");
    if (openit) close (con);
}

#' Add TSNE coordinates to an NG-CHM.
#'
#' Add TSNE coordinates as hidden covariate bars to an axis of an NG-CHM.  One hidden
#' covariate bar is added for each TSNE coordinate.  Coordinates have names 'BASENAME.coordinate.N',
#' where BASENAME is specified by the parameter basename (default TSNE) and N ranges from 1 to the number of
#' added covariate bars.
#'
#' pointIds is required because [Rtsne::Rtsne()] does not preserve the rownames of the data matrix it was applied to.
#' Their values must match those on that axis of the NGCHM, but their order must match those in the data
#' matrix passed to [Rtsne::Rtsne()].
#'
#' @examples
#' data(TCGA.GBM.EXPR, package='NGCHMDemoData');
#' rtc <- Rtsne::Rtsne(t(TCGA.GBM.EXPR));
#' hm <- chmNew ("gbm", TCGA.GBM.EXPR);
#' hm <- chmAddTSNE(hm, "column", rtc, colnames(TCGA.GBM.EXPR));
#'
#' @export
#'
#' @param hm The NGCHM to add the coordinates to
#' @param axis The NGCHM axis ("row" or "column") to add the coordinates to
#' @param tsne TSNE coordinates (output of [Rtsne::Rtsne()]) for the specified NGCHM axis
#' @param pointIds The NGCHM names for the data points in tsne
#' @param basename The prefix to use for the coordinate names.
#'
#' @return The NGCHM with added coordinates.
#' @seealso [chmAddPCA()]
#' @seealso [chmAddUMAP()]
#' @seealso [chmAddUWOT()]
#' @seealso [chmAddReducedDim()]

chmAddTSNE <- function (hm, axis, tsne, pointIds, basename = "TSNE") {
	stopifnot (is(hm, "ngchmVersion2"));
	stopifnot (mode(axis) == "character" && length(axis) == 1);
	stopifnot (axis == "row" || axis == "column");
	stopifnot (mode(pointIds) == "character" && length(pointIds) == nrow(tsne$Y));
	stopifnot (mode(basename) == "character" && length(basename) == 1);

	for (idx in 1:ncol(tsne$Y)) {
		coordname <- sprintf ("%s.coordinate.%d", basename, idx);
		vals <- tsne$Y[,idx];
		names(vals) <- pointIds;
		minv <- min (vals, na.rm=TRUE);
		maxv <- max (vals, na.rm=TRUE);
		midv <- if (minv*maxv < 0) 0.0 else (minv+maxv)/2.0;
		cmap <- chmNewColorMap(c(minv,midv,maxv), colors=c("#00007f","#d0d0d0","#7f0000"));
		cv <- chmNewCovariate(coordname, vals, cmap);
		hm <- chmAddCovariateBar(hm, axis, cv, display = "hidden");
	}
	return (hm);
}

#' Add PCA coordinates to an NG-CHM.
#'
#' Add PCA coordinates as hidden covariate bars to an axis of an NG-CHM.
#' One hidden covariate bar is added for each PCA coordinate (up to ndim coordinates).
#' Coordinates are given names 'BASENAME.coordinate.N', where BASENAME is specified by the
#' parameter basename (default "PC") and N ranges from 1 to the number of added covariate bars.
#'
#' @examples
#' data(TCGA.GBM.EXPR, package='NGCHMDemoData');
#' prc <- prcomp(TCGA.GBM.EXPR);
#' hm <- chmNew ("gbm", TCGA.GBM.EXPR);
#' hm <- chmAddPCA(hm, "column", prc);
#'
#' @export
#'
#' @param hm The NGCHM to add the coordinates to.
#' @param axis The NGCHM axis ("row" or "column") to add the coordinates to.
#' @param prc Principal component coordinates (output of [stats::prcomp()]) for the specified NGCHM axis.
#' @param basename The prefix to use for the coordinate names.
#' @param ndim The maximum number of coordinates to add.
#'
#' @return The NGCHM with added coordinates.
#' @seealso [chmAddTSNE()]
#' @seealso [chmAddUMAP()]
#' @seealso [chmAddUWOT()]
#' @seealso [chmAddReducedDim()]

chmAddPCA <- function (hm, axis, prc, basename = "PC", ndim=2) {
	stopifnot (is(hm, "ngchmVersion2"));
	stopifnot (mode(axis) == "character" && length(axis) == 1);
	stopifnot (axis == "row" || axis == "column");
	stopifnot (mode(basename) == "character" && length(basename) == 1);
	stopifnot (mode(ndim) == "numeric" && length(basename) == 1);

	pointIds <- rownames(prc$rotation);
	for (idx in 1:min(ncol(prc$rotation),ndim)) {
		coordname <- sprintf ("%s.coordinate.%d", basename, idx);
		vals <- prc$rotation[,idx];
		names(vals) <- pointIds;
		minv <- min (vals, na.rm=TRUE);
		maxv <- max (vals, na.rm=TRUE);
		midv <- if (minv*maxv < 0) 0.0 else (minv+maxv)/2.0;
		cmap <- chmNewColorMap(c(minv,midv,maxv), colors=c("#00007f","#d0d0d0","#7f0000"));
		cv <- chmNewCovariate(coordname, vals, cmap);
		hm <- chmAddCovariateBar(hm, axis, cv, display = "hidden");
	}
	return (hm);
}

#' Add UMAP coordinates to an NG-CHM.
#'
#' Add UMAP coordinates as hidden covariate bars to an axis of an NG-CHM.  One hidden
#' covariate bar is added for each UMAP coordinate.  Coordinates have names 'BASENAME.coordinate.N',
#' where BASENAME is specified by the parameter basename (default UMAP) and N ranges from 1 to the number of
#' added covariate bars.
#'
#' @examples
#' data(TCGA.GBM.EXPR, package='NGCHMDemoData');
#' umc <- umap::umap(t(TCGA.GBM.EXPR));
#' hm <- chmNew ("gbm", TCGA.GBM.EXPR);
#' hm <- chmAddUMAP(hm, "column", umc);
#'
#' @export
#'
#' @param hm The NGCHM to add the coordinates to.
#' @param axis The NGCHM axis ("row" or "column") to add the coordinates to.
#' @param umap TSNE coordinates (output of [umap::umap()]) for the specified NGCHM axis.
#' @param basename The prefix to use for the coordinate names.
#'
#' @return The NGCHM with added coordinates.
#' @seealso [chmAddPCA()]
#' @seealso [chmAddTSNE()]
#' @seealso [chmAddUWOT()]
#' @seealso [chmAddReducedDim()]

chmAddUMAP <- function (hm, axis, umap, basename = "UMAP") {
	stopifnot (is(hm, "ngchmVersion2"));
	stopifnot (mode(axis) == "character" && length(axis) == 1);
	stopifnot (axis == "row" || axis == "column");
	stopifnot (mode(basename) == "character" && length(basename) == 1);

	pointIds <- rownames(umap$layout);
	for (idx in 1:ncol(umap$layout)) {
		coordname <- sprintf ("%s.coordinate.%d", basename, idx);
		vals <- umap$layout[,idx];
		names(vals) <- pointIds;
		minv <- min (vals, na.rm=TRUE);
		maxv <- max (vals, na.rm=TRUE);
		midv <- if (minv*maxv < 0) 0.0 else (minv+maxv)/2.0;
		cmap <- chmNewColorMap(c(minv,midv,maxv), colors=c("#00007f","#d0d0d0","#7f0000"));
		cv <- chmNewCovariate(coordname, vals, cmap);
		hm <- chmAddCovariateBar(hm, axis, cv, display = "hidden");
	}
	return (hm);
}

#' Add UWOT::UMAP coordinates to an NG-CHM.
#'
#' Add UWOT::UMAP coordinates as hidden covariate bars to an axis of an NG-CHM.  One hidden
#' covariate bar is added for each UMAP coordinate.  Coordinates have names 'BASENAME.coordinate.N',
#' where BASENAME is specified by the parameter basename (default UMAP) and N ranges from 1 to the number of
#' added covariate bars.
#'
#' pointIds is required because [uwot::umap()] does not preserve the rownames of the data matrix it was applied to.
#' Their values must match those on that axis of the NGCHM, but their order must match those in the data
#' matrix passed to [uwot::umap()].
#'
#' @examples
#' data(TCGA.GBM.EXPR, package='NGCHMDemoData');
#' umc <- uwot::umap(t(TCGA.GBM.EXPR));
#' hm <- chmNew ("gbm", TCGA.GBM.EXPR);
#' hm <- chmAddUWOT(hm, "column", umc, colnames(TCGA.GBM.EXPR));
#'
#' @export
#'
#' @param hm The NGCHM to add the coordinates to.
#' @param axis The NGCHM axis ("row" or "column") to add the coordinates to.
#' @param uwot UMAP coordinates (output of [uwot::umap()]) for the specified NGCHM axis.
#' @param pointIds The NGCHM names for the data points in uwot
#' @param basename The prefix to use for the coordinate names.
#'
#' @return The NGCHM with added coordinates.
#' @seealso [chmAddPCA()]
#' @seealso [chmAddTSNE()]
#' @seealso [chmAddUMAP()]
#' @seealso [chmAddReducedDim()]

chmAddUWOT <- function (hm, axis, uwot, pointIds, basename = "UMAP") {
	stopifnot (is(hm, "ngchmVersion2"));
	stopifnot (mode(axis) == "character" && length(axis) == 1);
	stopifnot (axis == "row" || axis == "column");
	stopifnot (mode(pointIds) == "character" && length(pointIds) == nrow(uwot));
	stopifnot (mode(basename) == "character" && length(basename) == 1);

	for (idx in 1:ncol(uwot)) {
		coordname <- sprintf ("%s.coordinate.%d", basename, idx);
		vals <- uwot[,idx];
		names(vals) <- pointIds;
		minv <- min (vals, na.rm=TRUE);
		maxv <- max (vals, na.rm=TRUE);
		midv <- if (minv*maxv < 0) 0.0 else (minv+maxv)/2.0;
		cmap <- chmNewColorMap(c(minv,midv,maxv), colors=c("#00007f","#d0d0d0","#7f0000"));
		cv <- chmNewCovariate(coordname, vals, cmap);
		hm <- chmAddCovariateBar(hm, axis, cv, display = "hidden");
	}
	return (hm);
};

#' Generic method to get a dimensions matrix from obj.
#'
#' The return value must be NULL or a numeric matrix, each column of which is a (reduced) dimension.
#' The rows of the returned matrix must be named.
#'
#' @name getDimensions
#' @rdname getDimensions-method
#' @param obj The object from which to obtain the dimension(s).
#' @param ... Additional class-specific parameters for specifying the desired dimension.
#' @return A matrix with one dimension per column and one named row per observation in obj.
#' @export
#'
#' @seealso [chmAddReducedDim()]
#'
getDimensions <- function (obj, ...) {
    UseMethod ("getDimensions", obj);
};

getDimensions.default <- function (obj, ...) {
    return (NULL);
};

#' Add reduced dimension coordinates to an NG-CHM.
#'
#' Add (reduced) dimension coordinates from an object obj
#' as hidden covariate bars to an axis of an NG-CHM.  Depending on the object type, dimName and dimAxis
#' can be used to specify the name of the dimension of interest in obj.
#'
#' One hidden covariate bar is added for each coordinate obtained from `obj`.
#' If specified, maxDim limits the maximum number of covariate bars added to the chm.
#'
#' Coordinates have names 'BASENAME.coordinate.N', where BASENAME is specified by the parameter
#' basename (defaults to dimName if omitted) and N ranges from 1 to the number of added covariate bars.
#'
#' `obj` can be a numeric matrix, each column of which is a (reduced) dimension.  In this case, dimName and dimAxis
#' are not used for obtaining the reduced dimension.  The number of rows of the matrix must equal the size of the specified
#' NGCHM axis and each row of the matrix must be uniquely named using the names from that axis of the NG-CHM.
#'
#' `obj` can also be an instance of class className if there exists an S3 method getDimensions.className.
#' The method takes the object as its first parameter and up to two optional parameters, dimName and dimAxis,
#' that can be used to specify the desired dimension.  The method's return value is a matrix similar to
#' the one described in the preceding paragraph.  This package defines methods for classes `prcomp` and `umap`.
#'
#' @examples
#' hm <- chmAddReducedDim(hm, "column", obj, "PCA", 3, "PC");
#' hm <- chmAddReducedDim(hm, "column", obj, "TSNE");
#'
#' @export
#'
#' @param hm The NGCHM to add the coordinates to.
#' @param axis The NGCHM axis ("row" or "column") to add the coordinates to.
#' @param obj An object containing the (reduced) dimension.
#' @param dimName The name of the (reduced) dimension to create covariate bars for.
#' @param maxDim The maximum number of coordinates to add (default all).
#' @param basename The prefix to use for the coordinate names (defaults to dimName).
#' @param dimAxis The axis on obj containing the named dimension, if applicable.
#'
#' @return The NGCHM with added coordinates.
#' @seealso [chmAddPCA()]
#' @seealso [chmAddTSNE()]
#' @seealso [chmAddUMAP()]
#' @seealso [chmAddUWOT()]
#' @seealso [getDimensions()]

chmAddReducedDim <- function (hm, axis, obj, dimName, maxDim, basename, dimAxis) {
    stopifnot (is (hm, "ngchmVersion2"));
    stopifnot (is (axis, "character") && length(axis) == 1);

    # Initially try to use generic getDimension method.
    layout <- getDimensions (obj, dimName, dimAxis);
    # Try hard to find an applicable reducedDim method
    if (is.null(layout)) {
	pkg <- attr(class(obj), 'package');
	if (length(pkg) != 0) {
	    ns <- tryCatch (loadNamespace(pkg), error = function(e) e);
	    if (!is(ns, 'error') && exists('reducedDim',ns)) {
		if (missing(dimAxis)) {
		    layout <- get('reducedDim',ns) (obj, dimName);
		} else {
		    layout <- get('reducedDim',ns) (obj, dimName, axis=dimAxis);
		}
	    }
	}
    }
    if (is.null(layout)) {
	# Last chance: try .GlobalEnv
	if (exists ('reducedDim')) {
	    if (missing(dimAxis)) {
		layout <- get('reducedDim') (obj, dimName);
	    } else {
		layout <- get('reducedDim') (obj, dimName, axis=dimAxis);
	    }
	} else if (is (obj, "matrix")) {
	    layout <- obj;
	} else {
	    stop ("Unable to get reduced dimension from object");
	}
    }
    stopifnot (is (layout, "matrix"));
    if (missing(maxDim)) {
	maxDim <- ncol (layout);
    } else {
	stopifnot (maxDim <= ncol(layout));
    }
    if (missing(basename)) basename <- dimName;
    for (idx in 1:maxDim) {
        coordname <- sprintf ("%s.coordinate.%d", basename, idx);
	vals <- layout[,idx];
	minv <- min (vals, na.rm = TRUE);
	maxv <- max (vals, na.rm = TRUE);
	midv <- if (minv*maxv < 0) 0 else (minv+maxv)/2.0;
	cmap <- chmNewColorMap (c(minv, midv, maxv), colors=c("#00007f", "#d0d0d0", "#7f0000"));
	cv <- chmNewCovariate (coordname, vals, cmap)
	hm <- chmAddCovariateBar (hm, axis, cv, display="hidden");
    }
    return (hm);
};

#' @rdname getDimensions-method
#' @aliases getDimensions,prcomp
getDimensions.prcomp = function (obj, ...) {
    return (obj$rotation);
};

#' @rdname getDimensions-method
#' @aliases getDimensions,umap
getDimensions.umap = function (obj, ...) {
    return (obj$layout);
};

#' @rdname getDimensions-method
#' @aliases getDimensions,Seurat
#' @param dimName The name of the dimension matrix to obtain.
getDimensions.Seurat = function (obj, dimName, ...) {
    return (obj@reductions[[dimName]]@cell.embeddings);
};

#' Helper function to cast variables as integers.
#'
#' If variable value is far from integer, print error message and stop.
#'
#' @param variableToCast Variable to cast as integer
#' @return integer value of variableToCast
castAsInteger <- function(variableToCast) {
	roundTolerance = 0.01
	if (abs(round(variableToCast) - variableToCast) > roundTolerance) {
		log_error("Variable '",deparse(substitute(variableToCast)),"' must be integer")
		stop("Variable '",deparse(substitute(variableToCast)),"' must be integer")
	} else {
		return (as.integer(round(variableToCast)))
	}
}

#' Helper function to cast list as integer
#'
#' If variable value is far from integer, print error message and stop.
#'
#' @param listToCast List to cast as integer
#' @return list with members cast to integers
castListAsInteger <- function(listToCast) {
	roundTolerance = 0.01
	lapply(listToCast, function(elem) {
		if ((abs(round(elem) - elem)) > roundTolerance) {
			log_error("Entries of '",deparse(substitute(listToCast)),"' must be integer")
			stop("Entries of '",deparse(substitute(listToCast)),"' must be integer")
		}
	}) 
	return (as.integer(round(listToCast)))
}

#' Helper function to verify if variable is numeric.
#'
#' If not numeric, print error message and stop.
#'
#' @param variableToCheck The variable to check for being numeric.
#' @return TRUE
verifyNumeric <- function(variableToCheck) {
	if (!is.numeric(variableToCheck)) {
		log_error("Variable '",deparse(substitute(variableToCheck)),"' must be numeric.")
		stop("Variable '",deparse(substitute(variableToCheck)),"' must be numeric.")
	} else {
		return (TRUE)
	}
}

#' Creates new treeCuts object
#'
#' This function was designed to facilitate setting rowGapLocations and colGapLocations in the
#' [chmNew()] function. See examples section.
#'
#' @param numberOfCuts Number of tree cuts
#' @return [treeCuts-class] object with specified number of tree cuts
#' @export
#'
#' @examples 
#' mychm <- chmNew("test_chm", rowGapLocations=chmTreeGaps(5))
chmTreeGaps <- function(numberOfCuts) {
	return (new (Class="treeCuts", numberOfCuts=as.integer(numberOfCuts)))
}
