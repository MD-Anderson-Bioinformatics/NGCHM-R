
systemCheck <- function (command, ...) {
    # Execute the specified command and halt execution with an error
    # message if it fails.
    status <- system (command, ...)
    if (status != 0) {
        stop ('Error encountered executing system command: ', command)
    }
}

#' Register an ngchmServer.
#'
#' This function registers an ngchmServer that can be used when
#' making and installing a Next Generation Clustered Heat Map.
#'
#' @param uuid A string that identifies the server namespace.
#' @param server The ngchmServer to register.
#'
#' @export
#'
#' @seealso chmMake
#' @seealso chmInstall
#' @seealso chmUninstall
#' @seealso chmUnregisterServer
#' @seealso ngchmServer-class
#'
chmRegisterServer <- function (uuid, server) {
    obj <- list (uuid=uuid, name=server@name, server=server);
    matches <- which (vapply (ngchm.env$servers, function(srv) ((srv$uuid == uuid) && (srv$name == server@name)), TRUE));
    if (length (matches) > 0) {
	ngchm.env$servers[[matches]] <- obj;
    } else {
	ngchm.env$servers <- append (ngchm.env$servers, list(obj));
    }
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
#' @seealso chmRegisterServer
#' @seealso ngchmServer-class
#'
chmUnregisterServer <- function (uuid, name=NULL) {
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
#' @seealso chmMake
#' @seealso chmInstall
#' @seealso chmUninstall
#' @seealso ngchmServer-class
#'
chmServer <- function (name) {
    matches <- which (vapply (ngchm.env$servers, function(srv) (srv$name == name), TRUE));
    if (length (matches) > 0) {
	return (ngchm.env$servers[[matches[length(matches)]]]$server);
    } else {
	return (NULL);
    }
}

# Export for debug.
#' @export ngchmGetEnv
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
#' @param ... Zero or more initial objects to include in the NGCHM.
#'
#' @return An object of class ngchm
#'
#' @export
#'
#' @examples
#' mychm <- chmNew ("test_chm")
#'
#' @seealso ngchm-class
#' @seealso ngchmServer-class
#' @seealso chmAdd
#' @seealso chmMake
#' @seealso chmInstall

chmNew <- function (name, ...) {
    if (typeof (name) != "character") {
        stop (sprintf ("Parameter 'name' must have type 'character', not '%s'", typeof(name)));
    }
    if (length (name) != 1) {
        stop (sprintf ("Parameter 'name' must have a single value, not %d", length(name)));
    }
    if (nchar (name) == 0) {
        stop ("Parameter 'name' cannot be the empty string");
    }
    chm <- new (Class="ngchm", name=name)
    chm <- chmAddCSS (chm, 'div.overlay { border: 2px solid yellow; }');
    chm@rowOrder <- defaultRowOrder;
    chm@colOrder <- defaultColOrder;
    chm <- chmAddList (chm, list(...));
    chm
}

defaultColOrder <- function (chm) {
    if (length (chm@layers) == 0) stop ("chm requires at least one layer");
    as.dendrogram(hclust(as.dist(1-cor(chm@layers[[1]]@data)), method="ward"))
}

defaultRowOrder <- function (chm) {
    if (length (chm@layers) == 0) stop ("chm requires at least one layer");
    as.dendrogram(hclust(as.dist(1-cor(t(chm@layers[[1]]@data))), method="ward"))
}

# Function used by chmNew and chmAdd:
chmAddList <- function (chm, args) {
    #cat (sprintf ("chmAdd: %d items to add\n", length(args)), file=stderr());
    for (item in args) {
	cc <- class (item);
	if (cc == "ngchmLayer") { chm <- chmAddLayer (chm, item); }
	else if ((cc == "matrix") && (mode(item) == "numeric")) { chm <- chmAddLayer (chm, item); }
	else if (cc == "ngchmDataset") { chm <- chmAddDataset (chm, item); }
	else if (cc == "ngchmColormap") { chm <- chmAddColormap (chm, item); }
	else if (cc == "ngchmDialog") { chm <- chmAddDialog (chm, item); }
	else {
	    cat (sprintf ("Unable to add item of class '%s' to chm\n", class(item)));
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
#' @seealso ngchmLayer-class
#' @seealso chmNewColorMap
#' @seealso chmAddDataLayer
#' 
chmNewDataLayer <- function (label, data, colors=NULL) {
    if (typeof (label) != "character") {
        stop (sprintf ("Parameter 'label' must have type 'character', not '%s'", typeof(label)));
    }
    if (length (label) != 1) {
        stop (sprintf ("Parameter 'label' must have a single value, not %d", length(label)));
    }
    if (nchar (label) == 0) {
        stop ("Parameter 'label' cannot be the empty string");
    }
    if (!is.numeric (data)) {
        stop (sprintf ("Parameter 'data' for layer '%s' must be numeric", label));
    }
    if (length (dim(data)) != 2) {
        stop (sprintf ("Parameter 'data' for layer '%s' must have exactly 2 dimensions, not %d", label, length(dim(data))));
    }
    if (length (rownames(data)) == 0) {
        stop (sprintf ("Parameter 'data' for layer '%s' must have rownames set", label));
    }
    if (length (colnames(data)) == 0) {
        stop (sprintf ("Parameter 'data' for layer '%s' must have colnames set", label));
    }
    if (length(colors) == 0)
	colors <- chmNewColorMap (data, c("green", "black", "red"));
    new (Class="ngchmLayer", name=label, data=data, colors=colors)
}

#' Create a new Dataset for a NGCHM.
#'
#' This function creates a new Dataset suitable for attaching to a Next Generation Clustered Heat Map.
#'
#' @param name The filename prefix under which the dataset will be saved to the ngchm.
#' @param description A description of the dataset.
#' @param data A matrix containing the data in the dataset. Must have rownames and colnames.
#' @param row.covariates An optional list of row covariates.
#' @param column.covariates An optional list of column covariates.
#'
#' @return An object of class ngchmDataset
#'
#' @export
#'
#' @seealso ngchmDataset-class
#' @seealso ngchmCovariate-class
#' @seealso chmAddDataset
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
    if (!is.numeric (data)) {
        stop (sprintf ("Parameter 'data' for dataset '%s' must be numeric", name));
    }
    if (length (dim(data)) != 2) {
        stop (sprintf ("Parameter 'data' for dataset '%s' must have exactly 2 dimensions, not %d", name, length(dim(data))));
    }
    if (length (rownames(data)) == 0) {
        stop (sprintf ("Parameter 'data' for dataset '%s' must have rownames set", name));
    }
    if (length (colnames(data)) == 0) {
        stop (sprintf ("Parameter 'data' for dataset '%s' must have colnames set", name));
    }
    if (length (row.covariates) > 0) {
        if (class(row.covariates) == "ngchmCovariate") {
	    row.covariates <- list (row.covariates);
	} else if (class(row.covariates) == "list") {
	    stopifnot (all (vapply (row.covariates, function(cov)class(cov)=="ngchmCovariate", TRUE)));
	} else {
	    stop (sprintf ("Parameter 'row.covariates' for dataset '%s' must be either a covariate or list of covariates, not a '%s'", name, class(row.covariates)));
	}
    }
    if (length (column.covariates) > 0) {
        if (class(column.covariates) == "ngchmCovariate") {
	    column.covariates <- list (column.covariates);
	} else if (class(column.covariates) == "list") {
	    stopifnot (all (vapply (column.covariates, function(cov)class(cov)=="ngchmCovariate", TRUE)));
	} else {
	    stop (sprintf ("Parameter 'column.covariates' for dataset '%s' must be either a covariate or list of covariates, not a '%s'", name, class(column.covariates)));
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
#' @param label The short R-compatible identifier used to identify the covariate (derived from fullname if not specified).
#'
#' @return An object of class ngchmCovariate.
#'
#' @export
#'
#' @seealso ngchmCovariate-class
#' @seealso chmAddCovariate
#' @seealso chmNewColorMap
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

    if ((length (value.properties) > 0) && (class(value.properties) != "ngchmColormap")) {
	stop (sprintf ("value.properties for covariate '%s' must have class ngchmColormap, not '%s'", fullname, class(value.properties)));
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
    new (Class="ngchmCovariate", label=covabbv, fullname=fullname, type=type,
	 label.series = values,
	 series.properties=value.properties);
}

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
#'
#' @return An object of class ngchmBar
#'
#' @export
#'
#' @examples
#' bar.data <- ifelse (rnorm(1000) < 0, "negative", "non-negative")
#' bar.colors <- chmNewColorMap (c("negative", "non-negative"),
#'                               c("white", "black"), missing.color='red')
#' bar <- chmNewClassBar ("Group", "discrete", bar.data, bar.colors)
#'
#' @seealso ngchmBar-class
#' @seealso chmNewColorMap
#' @seealso chmNewCovariateBar
#' @seealso chmAddCovariateBar
#' @seealso chmAddClassBar
#'
chmNewClassBar <- function (label, type, data, colors=NULL, display="visible", thickness=as.integer(10), merge=NULL) {
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
        stop (sprintf ("Parameter 'type' for classbar '%s' must have type 'character', not '%s'", label, typeof(type)));
    }
    if (length (type) != 1) {
        stop (sprintf ("Parameter 'type' for classbar '%s' must have a single value, not %d", label, length(type)));
    }
    if (!(type %in% c("discrete", "continuous"))) {
        stop (sprintf ("Parameter 'type' for classbar '%s' must be either 'discrete' or 'continuous', not '%s'", label, type));
    }
    if (typeof (display) != "character") {
        stop (sprintf ("Parameter 'display' for classbar '%s' must have type 'character', not '%s'", label, typeof(display)));
    }
    if (length (display) != 1) {
        stop (sprintf ("Parameter 'display' for classbar '%s' must have a single value, not %d", label, length(display)));
    }
    if (!(display %in% c("visible", "hidden")))
        stop (sprintf ("Parameter 'display' for classbar '%s' must be either 'visible' or 'hidden', not '%s'", label, display));
    if ((display == "visible") && (length(colors) == 0)) {
        if (type == "discrete") {
	    qq <- sort (unique (data));
	} else {
	    qq <- quantile (data);
	}
	colors <- rainbow(length(qq),start=2/6,end=0);
	colors <- vapply (colors, function(cc)substr(cc,1,7), "");
	colors <- chmNewColorMap (qq, colors);
    }
    if ((length(merge) > 0) && !(merge %in% c("average", "peakColor", "specialColor", "mostCommon")))
        stop (sprintf ("Unknown classbar merge value '%s'. Must be 'average', 'peakColor', 'specialColor' or 'mostCommon'", merge));
    new (Class="ngchmBar", label=label, type=type, data=data, thickness=thickness, colors=colors, display=display, merge=merge)
}

#' Create a new covariate Bar for a NGCHM
#'
#' This function creates a new covariate bar suitable for adding to a Next Generation Clustered Heat Map.
#'
#' @param covar The covariate to be displayed in the bar.
#' @param display Whether the covariate bar will be "hidden" or "visible" (default).
#' @param thickness The thickness of the covariate bar in pixels. (Default 10).
#' @param merge Algorithm for merging covariates when necessary ("average", "peakColor", "specialColor", or "mostCommon").
#'
#' @return An object of class ngchmBar
#'
#' @export
#'
#' @seealso ngchmBar-class
#' @seealso chmNewColorMap
#' @seealso chmAddCovariateBar
#'
chmNewCovariateBar <- function (covar, display="visible", thickness=as.integer(10), merge=NULL)
{
    chmNewClassBar (covar@fullname, covar@type, covar@label.series, covar@series.properties,
		    display=display, thickness=thickness, merge=merge)
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
#' @param missing A string specifying the color to use for missing data.
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
#' @seealso ngchmColormap-class
#' @seealso chmNewDataLayer
#' @seealso chmNewClassBar
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
    if (class(values) == 'matrix') {
	# User just supplied a data matrix.
	if (NC == 0) NC <- 3;
        if (type == "quantile") {
	    values <- (1.0/(NC-1)) * (0:(NC-1));
	} else if (type == "linear") {
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
    } else if (class(values) %in% c('character', 'numeric', 'integer', 'logical')) {
	# User supplied a vector of values.
	if (anyDuplicated(values) != 0) {
	    stop ("chmNewColorMap: values contains duplicates");
	}
	if (NC == 0) {
	    NC <- length (values);
	}
	if (length(values) != NC)
	    stop (sprintf ("chmNewColorMap: number of values (%d) does not equal number of color (%d). It should.", length(values), NC));
    } else {
	# Don't know what the user provided.
        stop (sprintf ("chmNewColorMap: values vector has unknown class '%s'. It must be either a vector or a matrix.",
	               class (values)));
    }
    stopifnot (length(values) == NC);

    # Auto-pick colors if needed.
    if (length(colors) == 0) {
	if (length(palette) > 0) {
	    colors <- palette(NC);
	} else {
	    colors <- rainbow(NC,start=2/6,end=0);
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
    col2rgb (missing.color);  # error check

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
    col2rgb (color);  # error check
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
#' @param global A logical: TRUE if should be defined globally, not within a customization section. (Default FALSE.)
#'
#' @return An object of class ngchmJS
#'
#' @export
#'
#' @examples
#' alertFn <- chmNewFunction ("showAlert", "Display the parameter in an alert box",
#'                            "function showAlert(label) { alert(label); }", TRUE)
#' dbLookup <- chmNewFunction ("dbLookup", "Lookup the parameter in a database",
#'                            "function showAlert(database, label) { alert(database[label]); }", c("database"))
#'
#' @seealso ngchmJS-class
#' @seealso chmAddMenuItem
#' @seealso chmBindFunction
#' @seealso chmRegisterFunction
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
#' @seealso ngchm-class
#' @seealso chmAddProperty
#'
chmNewProperty <- function (label, value) {
    if (typeof (label) != "character") {
        stop (sprintf ("Parameter 'label' must have type 'character', not '%s'", typeof(label)));
    }
    if (length (label) != 1) {
        stop (sprintf ("Parameter 'label' must have a single value, not %d", length(label)));
    }
    if (nchar (label) == 0) {
        stop ("Parameter 'label' cannot be the empty string");
    }
    if (typeof (value) != "character") {
        stop (sprintf ("Parameter 'value' for property '%s' must have type 'character', not '%s'", label, typeof(value)));
    }
    if (length (value) != 1) {
        stop (sprintf ("Parameter 'value' for property '%s' must have a single value, not %d", label, length(value)));
    }
    new (Class="ngchmProperty", label=label, value=value)
}

#' Create a new object representing a NGCHM server. 
#'
#' This function creates a new object that represents a NGCHM server.
#'
#' @param serverName The DNS name of the NGCHM server.
#' @param username The login name to use when deploying to the server (defaults to the username ssh uses).
#' @param keypath The path to the key to use when deploying to the server (defaults to your normal ssh credentials).
#' @param serverPort The port on which the server is listening.
#' @param deployServer The DNS name to use when deploying a NGCHM (defaults to serverName).
#' @param deployDir The directory on the server in which to place a NGCHM when deploying (defaults to /chmData).
#' @param jarFile The location of the heatmap build jar file to use when making a NGCHM (defaults to jar file on deployServer).
#' @param urlBase The base URL used to access a deployed NGCHM (defaults to serverName:serverPort/chm/chm.html).
#'
#' @return An object of class ngchmServer
#'
#' @export
#'
#' @examples
#' cloudServ <- chmNewServer ("dnsname.domain")
#'
#' @seealso ngchmServer-class
#' @seealso chmMake
#' @seealso chmInstall
#' @seealso chmUninstall
#'
chmNewServer <- function (serverName, username=NULL, keypath=NULL, serverPort=8080, deployServer=NULL, deployDir=NULL,
                       jarFile=NULL, urlBase=NULL)
{
    if (is.null (deployServer)) deployServer = serverName;
    if (is.null (deployDir)) deployDir = "/chmData";
    if (is.null (jarFile)) jarFile = paste ("http://", deployServer, ":", serverPort, "/chm/resources/heatmappipeline.jar", sep="");
    if (is.null (urlBase)) urlBase = paste ("http://", serverName, ":", serverPort, "/chm/chm.html", sep="");
    new (Class="ngchmServer", 
	 deployServer = deployServer,
	 deployDir = deployDir,
	 username = username,
	 keypath = keypath,
	 jarFile = paste ("http://", deployServer, ":", serverPort, "/chm/resources/heatmappipeline.jar", sep=""),
	 urlBase = paste ("http://", serverName, ":", serverPort, "/chm/chm.html", sep=""));
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
#' @seealso chmAddMenuItem
#' @seealso chmNewFunction
#' @seealso ngchmFunction-class
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
#' @seealso chmAddAxisType
#' @seealso chmRegisterMatrixFunction
#' @seealso chmRegisterTypeMapper
#' @seealso chmNewFunction
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
    if (class(type) != "character")
        stop (sprintf ("chmRegisterAxisFunction: error registering axis function '%s'. Specified type is '%', not string.", label, class(type)));
    if (class(label) != "character")
        stop (sprintf ("chmRegisterAxisFunction: error registering axis function. Type of specified label is '%', not string.", class(type)));
    if (class(fn) == "character")
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
#' @seealso chmAddAxisType
#' @seealso chmRegisterAxisFunction
#' @seealso chmRegisterTypeMapper
#' @seealso chmNewFunction
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
    if (class(fn) == "character")
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
#' @seealso chmListTypes
#' @seealso chmGetTypeInfo
#' @seealso chmRegisterTypeMapper
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
#' @seealso chmListTypes
#' @seealso chmRegisterType
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
#' @export
#'
#' @seealso chmGetTypeInfo
print.ngchm.type.info <- function (ti) {
    cat (sprintf ("NGCHM type %s: %s\n", ti$name, ti$description));
    if (length (ti$axisFunctions) > 0) {
	fns <- paste (vapply (ti$axisFunctions, function(af)af@label, ""), collapse=", ");
	cat (sprintf ("matches axis functions %s.\n", fns));
    }
    if (length (ti$matrixFunctions) > 0) {
	fns <- paste (vapply (ti$matrixFunctions, function(af)af@label, ""), collapse=", ");
	cat (sprintf ("matches matrix functions %s.\n", fns));
    }
    if (length (ti$typeMappers) > 0) {
	types <- paste (vapply (ti$typeMappers, function(af)af@totype, ""), collapse=", ");
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
#' @param name The function's javascript name.
#' @param fn The Javascript function to register.
#'
#' @return NULL
#'
#' @export
#'
#' @seealso chmAddAxisType
#' @seealso chmRegisterAxisFunction
#' @seealso chmRegisterMatrixFunction
#' @seealso chmNewFunction
chmRegisterTypeMapper <- function (fromtype, totype, fn) {
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
    if (class(fn) == "character")
        fn <- chmGetFunction (fn);
    newtm <- new ("ngchmTypeMapper", fromtype=fromtype, totype=totype, func=fn);
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
#' @seealso chmAddMenuItem
#' @seealso chmNewFunction
#' @seealso ngchmFunction-class
#'
chmRegisterFunction <- function (fn) {
    if (class(fn) != "ngchmJS") {
        stop (sprintf ("Parameter 'fn' must have type 'ngchmJS', not '%s'", class(fn)));
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
#' @param installMethod 
#'
#' @export

chmCreateServerProtocol <- function (protocolName,
                                     installMethod, uninstallMethod,
	                             makePrivate, makePublic) {
    if (typeof (protocolName) != "character") {
        stop (sprintf ("Parameter 'protocolName' must have type 'character', not '%s'", typeof(protocolName)));
    }
    if (length (protocolName) != 1) {
        stop (sprintf ("Parameter 'protocolName' must have a single value, not %d", length(protocolName)));
    }
    if (nchar (protocolName) == 0) {
        stop ("Parameter 'protocolName' cannot be the empty string");
    }
    dm <- new (Class="ngchmServerProtocol", protocolName=protocolName,
	       installMethod=installMethod, uninstallMethod=uninstallMethod,
	       makePrivate=makePrivate, makePublic=makePublic);
    matches <- which (vapply (ngchm.env$serverProtocols, function(ss) (ss@protocolName == protocolName), TRUE));
    if (length (matches) > 0) {
	ngchm.env$serverProtocols[[matches]] <- dm;
    } else {
	ngchm.env$serverProtocols <- append (ngchm.env$serverProtocols, dm);
    }
    dm
}

#' Lookup a Server Protocol
#'
#' @export
#'
#' @param protocolName The name of the server protocol to lookup

chmGetServerProtocol <- function (protocolName) {
    if (typeof (protocolName) != "character") {
        stop (sprintf ("Parameter 'protocolName' must have type 'character', not '%s'", typeof(protocolName)));
    }
    if (length (protocolName) != 1) {
        stop (sprintf ("Parameter 'protocolName' must have a single value, not %d", length(protocolName)));
    }
    if (nchar (protocolName) == 0) {
        stop ("Parameter 'protocolName' cannot be the empty string");
    }
    matches <- which (vapply (ngchm.env$serverProtocols, function(ss) (ss@protocolName == protocolName), TRUE));
    if (length(matches) == 0) {
        stop (sprintf ("No server protocol found with name '%s'", protocolName));
    } else {
	return (ngchm.env$serverProtocols[[matches]]);
    }
}

#' List the predefined Javascript functions available for use in NGCHM menus.
#'
#' This function lists the predefined Javascript functions available for use in NGCHM menus.
#'
#' @param re Only functions with names matching re are printed (default ".*")
#'
#' @export
#'
#' @seealso chmAddMenuItem
#' @seealso chmGetFunction
#' @seealso chmRegisterFunction
#' @seealso grep
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
#' @seealso chmAddAxisType
#' @seealso chmGetFunction
#' @seealso chmListFunctions
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
#' @seealso chmGetFunction
#' @seealso chmListFunctions
#' @seealso chmRegisterTypeMapper
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
    chmRegisterTypeMapper (listtype, itemtype, fn);
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
#' @seealso chmAddAxisType
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
#' @seealso chmNewFunction
#' @seealso ngchmFunction-class
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
    if (class(jsfn) != "ngchmJS") {
        stop (sprintf ("a toolbox function must be of class ngchmJS (see chmNewFunction) not '%s'",
	               class(jsfn)));
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
    if (length (chm@layers) > 0) {
        # Check new layer is compatible with first layer.
	layer1 <- chm@layers[[1]];
	if (any (dim(layer1@data) != dim(layer@data))) {
	    stop (sprintf ('Dimensions of new layer "%s" (%dx%d) for CHM "%s" differ from those of existing layers (%dx%d)',
	                   layer@name, nrow(layer@data), ncol(layer@data), chm@name, nrow(layer1@data), ncol(layer1@data)));
	}
	nm <- rownames (layer@data);
	nm1 <- rownames (layer1@data);
	if (!setequal (nm, nm1)) {
	    m <- sprintf ('Row names of new layer "%s" for CHM "%s" differ from those of existing layers',
			   layer@name, chm@name);
	    errs <- namesdifferror ('new layer', nm, 'existing layers', nm1);
	    stop (paste (c (m, errs), collapse="\n"));
	}
	nm <- colnames (layer@data);
	nm1 <- colnames (layer1@data);
	if (!setequal (nm, nm1)) {
	    m <- sprintf ('Column names of new layer "%s" for CHM "%s" differ from those of existing layers',
			   layer@name, chm@name);
	    errs <- namesdifferror ('new layer', nm, 'existing layers', nm1);
	    stop (paste (c (m, errs), collapse="\n"));
	}
    } else {
        # First layer.  Check names are compatible with class bars, if any.
	layername <- sprintf ('new layer "%s"', layer@name);
	if (length (chm@rowClassbars) > 0) {
	    for (ii in 1:length(chm@rowClassbars)) {
	        validateClassbar (chm, "Row", layername, rownames(layer@data), chm@rowClassbars[[ii]]);
	    }
	}
	if (length (chm@colClassbars) > 0) {
	    for (ii in 1:length(chm@colClassbars)) {
	        validateClassbar (chm, "Column", layername, colnames(layer@data), chm@colClassbars[[ii]]);
	    }
	}
    }
}

validateNewClassbar <- function (chm, where, bar)
{
    if (length (chm@layers) > 0) {
	layer <- chm@layers[[1]];
	layername <- sprintf ('layer "%s"', layer@name);
	if (where %in% c("row", "both")) {
	    validateClassbar (chm, "Row", layername, rownames(layer@data), bar);
	}
	if (where %in% c("column", "both")) {
	    validateClassbar (chm, "Column", layername, colnames(layer@data), bar);
	}
    }
}

validateClassbar <- function (chm, where, layername, labels, bar)
{
    if (length (intersect (labels, names(bar@data))) == 0) {
	m <- sprintf ('%s names of %s for CHM "%s" are completely different from those of covariate bar "%s"',
		       where, layername, chm@name, bar@label);
	errs <- namesdifferror (layername, labels, sprintf ('covariate bar "%s"', bar@label), names(bar@data));
	stop (paste (c (m, errs), collapse="\n"));
    }
}

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
#' @seealso chmAdd
#' @seealso chmAddDialog
#' 
chmNewDialog <- function (id, title, fn) {
    if (class(fn) == "character") {
        fn <- chmGetFunction (fn);
    }
    dialog <- new (Class="ngchmDialog", id=id, title=title, fn=fn);
    dialog
}

#' Return the names of the NGCHM servers defined to date in this session.
#'
#' @export

chmListServers <- function () {
    vapply (ngchm.env$servers, function (svr) svr$name, "")
}

getBuilderJar <- function (server) {
    
    if (!exists (server@jarFile, ngchm.env$jarCache)) {
        # Load server@jarFile into jarCache
	if (length(grep("^scp://", server@jarFile)) > 0) {
	    tmpJarFile <- system2 ("mktemp", args=c("-p", tempdir(), "hmtXXXXXXXXX.jar"), stdout=TRUE);
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
	    tmpJarFile <- system2 ("mktemp", args=c("-p", tempdir(), "hmtXXXXXXXXX.jar"), stdout=TRUE);
	    systemCheck (sprintf ("wget -q -O %s %s", tmpJarFile, shQuote (server@jarFile)));
	    ngchm.env$jarCache[[server@jarFile]] <- tmpJarFile;
	}
	else if (length(grep("^https://", server@jarFile)) > 0) {
	    tmpJarFile <- system2 ("mktemp", args=c("-p", tempdir(), "hmtXXXXXXXXX.jar"), stdout=TRUE);
	    systemCheck (sprintf ("wget -q -O %s %s", tmpJarFile, shQuote (server@jarFile)));
	    ngchm.env$jarCache[[server@jarFile]] <- tmpJarFile;
	}
	else if (length(grep("^file://", server@jarFile)) > 0) {
	    ngchm.env$jarCache[[server@jarFile]] <- substring (server@jarFile, 8);
	} else {
	    stop (sprintf ("Unknown retrieval method for jarFile '%s' for NGCHM server '%s'", chm@jarFile, server@name));
	}
    }
    ngchm.env$jarCache[[server@jarFile]]
}

#' Create an ngchmServer object for an MDS2 deployment server directory.
#'
#' Create an ngchmServer object called 'servername' for the MDS2 NGCHM deployment directory 'cfgDir' on
#' server 'deployServer'.  If deployServer is NULL (default), 'cfgDir' is assummed to be on the local machine.
#'
#' @param servername The name of the new server object.
#' @param cfgDir The absolute path of the MDS2 directory on the deployServer.
#' @param cfgServer The server on which cfgDir is located.
#'
#' @export

chmCreateServer <- function (servername,
                             cfgDir = NULL,
			     cfgServer = NULL,
			     theJarFile = NULL,
			     serverURL = NULL) {
    cfg <- new.env();
    cfg$traceLevel <- "PROGRESS";
    cfg$serverProtocol <- "manual";
    cfg$deployServer <- cfgServer;
    cfg$deployDir <- NULL;
    cfg$username <- NULL;
    cfg$keypath <- NULL;
    cfg$jarFile <- NULL;

    # jarURL is last-ditch guess.
    if (length(serverURL) == 0) {
	if (length (cfgServer) == 0) {
            serverURL <- "http://fix.me.in.the.config.file/chm";
	    jarURL <- NULL;
	} else {
            serverURL <- paste ("http:", cfgServer, "/chm", sep="");
	    jarURL <- paste (serverURL, "resources", "heatmappipeline.jar", sep="/");
	}
    } else if (substr (serverURL, 1, 2) == "//") {
        serverURL <- paste ("http:", serverURL, sep="");
	jarURL <- paste (serverURL, "resources", "heatmappipeline.jar", sep="/");
    } else if (substr (serverURL, 1, 7) == "http://") {
	jarURL <- paste (serverURL, "resources", "heatmappipeline.jar", sep="/");
    } else if (substr (serverURL, 1, 8) == "https://") {
	jarURL <- paste (serverURL, "resources", "heatmappipeline.jar", sep="/");
    } else {
	jarURL <- NULL;
    }
    cfg$urlBase <- paste (serverURL, "chm.html", sep="/");

    if (length(cfgServer) == 0) {
	if ((length(cfgDir) > 0) && file.exists (cfgDir)) {
	    cfg$serverProtocol <- "mds2";
	    cfg$deployDir <- cfgDir;
	    if (length (theJarFile) == 0) {
		jarFile <- file.path (cfgDir, ".mds", "heatmappipeline.jar");
		if (file.exists (jarFile)) {
		    cfg$jarFile <- paste ("file://", jarFile, sep="");
		}
	    }
	    readConfigFile (cfg, file.path (cfgDir, "config.txt"), '=');
	}
    } else if (length (cfgDir) > 0) {
	cfg$serverProtocol <- "mds2";
	cfg$deployServer <- cfgServer;
	cfg$deployDir <- cfgDir;
	cfgFile <- system2 ("mktemp", args=c("-p", tempdir(), "cfgXXXXXXXXX.txt"), stdout=TRUE);
	rcfg <- new.env();
	if (system2 ("scp", args=c(sprintf ("%s:%s/%s", cfgServer, cfgDir, "config.txt"), cfgFile)) == 0) {
	    readConfigFile (rcfg, cfgFile, '=');
	}
	if (length (theJarFile) == 0) {
	    # No user specified jarFile, so let's see what the remote config says.
	    if (length (rcfg$jarFile) == 0) {
		# No jarFile specified in remote config, so peek to see if there's a remote jarFile in .mds subdirectory.
		rcfg$jarFile <- paste (cfgDir, ".mds", "heatmappipeline.jar", sep="");
		found <- system2 ("ssh", args=c(cfgServer, sprintf ("[ -r %s ]", rcfg$jarFile)));
		if (found == 0) {
		    # Shell command above succeeded.
		    cfg$jarFile <- paste ("scp://", cfgServer, rcfg$jarFile, sep="");
		}
	    }
	    else if (substr (rcfg$jarFile, 1, 7) == "file://") {
		# Remote specified jarFile using file://, so convert to an scp://
		rcfg$jarFile <- substr (rcfg$jarFile, 8, nchar (rcfg$jarFile));
		if (substr (rcfg$jarFile, 1, 1) != '/') {
		    rcfg$jarFile <- paste ('', cfgDir, rcfg$jarFile, sep="/");
		}
		found <- system2 ("ssh", args=c(cfgServer, sprintf ("[ -r %s ]", rcfg$jarFile)));
		if (found == 0) {
		    cfg$jarFile <- paste ("scp://", cfgServer, rcfg$jarFile, sep="");
		}
	    }
	    else {
		# We assume network based addresses will work equally well from the current machine.
		cfg$jarFile <- rcfg$jarFile;
	    }
	}
	if (length(rcfg$username) != 0) cfg$username <- rcfg$username;
	if (length(rcfg$keypath) != 0) cfg$keypath <- rcfg$keypath;
	if (length(rcfg$serverProtocol) != 0) cfg$serverProtocol <- rcfg$serverProtocol;
	if (length(rcfg$deployServer) != 0) cfg$deployServer <- rcfg$deployServer;
	if (length(rcfg$deployDir) != 0) cfg$deployDir <- rcfg$deployDir;
	if (length(rcfg$urlBase) != 0) cfg$urlBase <- rcfg$urlBase;
	if (length(rcfg$traceLevel) != 0) cfg$traceLevel <- rcfg$traceLevel;
    }

    if (length (theJarFile) > 0) {
	# the user explicitly specified theJarFile.  Use it or fail.
	if (file.exists (theJarFile)) {
	    cfg$jarFile <- paste ("file://", theJarFile, sep="");
	}
	else {
	    stop (sprintf ("Specified jarFile '%s' does not exist", theJarFile));
	}
    }
    else if ((length(cfg$jarFile) == 0) && (length(jarURL) > 0)) {
	# No info in config directory, but jarURL is a guess. Try it.
	try (suppressWarnings({
	    if (system2 ("wget", args=c("-q", "--spider", jarURL)) == 0) {
		cfg$jarFile <- jarURL;
	    }
	}), silent=TRUE);
    }

    if (length (cfg$jarFile) == 0) {
        stop ("Error creating NGCHM server: no heatmappipeline.jar was found");
    }

    chmRegisterServer("base", new(Class="ngchmServer", 
			   name = servername,
			   traceLevel = cfg$traceLevel,
			   username = cfg$username,
			   keypath = cfg$keypath,
			   serverProtocol = chmGetServerProtocol (cfg$serverProtocol),
			   deployServer = cfg$deployServer,
			   deployDir = cfg$deployDir,
			   jarFile = cfg$jarFile,
			   urlBase = cfg$urlBase));
}

readConfigFile <- function (cfg, filename, sep=':') {
    lines <- NULL;
    try (suppressWarnings(lines <- readLines (filename)), silent=TRUE);
    if (length (lines) > 0) {
	for (line in strsplit (lines, sep)) {
	    if (length(line) != 2) error (sprintf ('Malformed configuration line "%s" in %s: should be option%svalue', paste(line,sep=sep), filename, sep));
	    cfg[[line[1]]] <- line[2];
	}
    }
}
