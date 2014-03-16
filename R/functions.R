
ngchm.env <- new.env()

ngchm.env$scripts <- c()
ngchm.env$axisFunctions <- NULL
ngchm.env$matrixFunctions <- NULL
ngchm.env$typeMappers <- NULL
ngchm.env$serverProtocols <- NULL
ngchm.env$toolbox <- NULL

# Export for debug.
#' @export ngchmGetEnv
ngchmGetEnv <- function () {
    return (ngchm.env);
}

#' Create a new NGCHM.
#'
#' This function creates a Next Generation Clustered Heat Map (NGCHM) object in memory.
#' The bare NGCHM needs at least one data layer added to it before it can be compiled.
#'
#' @param name The name under which the NGCHM will be saved to the NGCHM server.
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
#' @seealso chmMake
#' @seealso chmInstall

chmNew <- function (name) {
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
#' noise.colors <- chmNewColorMap ("linear", "yellow", c(0,1,2),
#'                                 c("green", "black", "red"))
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
	colors <- chmNewColorMap ("linear", "white", data, c("green", "black", "red"));
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
    new (Class="ngchmDataset", name=name, description=description, data=data,
         row.covariates = row.covariates,
	 column.covariates=column.covariates);
}

#' Create a new Covariate for adding to an NGCHM auxilary dataset.
#'
#' This function creates a new Covariate suitable for attaching to an NGCHM auxilary dataset.
#'
#' @param label The short label used to identify the covariate within its dataset.
#' @param fullname The full (human readable) name of the covariate.
#' @param label.series The series to which each label in the dataset belongs.
#' @param column.series.properties A list of mappings from series names to property values.
#'
#' @return An object of class ngchmCovariate.
#'
#' @export
#'
#' @seealso ngchmCovariate-class
#' @seealso chmAddCovariate
#' 
chmNewCovariate <- function (label, fullname, label.series, series.properties) {
    if (typeof (label) != "character") {
        stop (sprintf ("Parameter 'label' must have type 'character', not '%s'", typeof(label)));
    }
    if (length (label) != 1) {
        stop (sprintf ("Parameter 'label' must have a single value, not %d", length(label)));
    }
    if (nchar (label) == 0) {
        stop ("Parameter 'label' cannot be the empty string");
    }
    if (typeof (fullname) != "character") {
        stop (sprintf ("Parameter 'fullname' for covariate '%s' must have type 'character', not '%s'", label, typeof(fullname)));
    }
    if (length (fullname) != 1) {
        stop (sprintf ("Parameter 'fullname' for covariate '%s' must have a single value, not %d", label, length(fullname)));
    }
    if (nchar (fullname) == 0) {
        stop (sprintf ("Parameter 'fullname' for covariate '%s' cannot be the empty string", label));
    }
    new (Class="ngchmCovariate", label=label, fullname=fullname,
	 label.series = label.series,
	 series.properties=series.properties);
}

#' Create a new Classification Bar for a NGCHM
#'
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
#' bar.colors <- chmNewColorMap ("linear", "red",
#'                               c("negative", "non-negative"),
#'                               c("white", "black"))
#' bar <- chmNewClassBar ("Group", "discrete", bar.data, bar.colors)
#'
#' @seealso ngchmBar-class
#' @seealso chmNewColorMap
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
    if ((display == "visible") && (length(colors) == 0))
        stop (sprintf ("Classbar '%s' is visible so it requires a color map"), label);
    if ((length(merge) > 0) && !(merge %in% c("average", "peakColor", "specialColor", "mostCommon")))
        stop (sprintf ("Unknown classbar merge value '%s'. Must be 'average', 'peakColor', 'specialColor' or 'mostCommon'", merge));
    new (Class="ngchmBar", label=label, type=type, data=data, thickness=thickness, colors=colors, display=display, merge=merge)
}

#' Create a new Color Map for use in constructing a NGCHM
#'
#' This function creates a new Color Map suitable for use in constructing Data Layers and Classification Bars
#' in Next Generation Clustered Heat Map.
#' If values is a matrix, the function will estimate a suitable sequence of color break points.  For a quantile
#' color map, the matrix data is ignored.  For a linear color map, it will use equispaced values between a low value and
#' a high value. The low value is the median of the minima of each row in the matrix, and the high value is the median
#' of the row maxima. If the low and high values have different signs, the values will be symmetric about zero.
#'
#' @param type The string "linear" or the string "quantile" (or unique abbreviation thereof).
#' @param missing A string specifying the color to use for missing data.
#' @param values A vector specifying the break points for which the following colors are defined, or a data matrix.
#' @param colors A vector of strings specifying the color to use for each color break point.
#'
#' @return An object of class ngchmColormap
#'
#' @export
#'
#' @examples
#' noise.colors <- chmNewColorMap ("linear", "yellow", c(0,1,2),
#'                                 c("green", "black", "red"))
#' bar.colors <- chmNewColorMap ("quantile", "white", c("small", "big"),
#'                               c("#00FFFF", "#FF00FF"))
#'
#' @seealso ngchmColormap-class
#' @seealso chmNewDataLayer
#' @seealso chmNewClassBar
#'
chmNewColorMap <- function (type, missing, values, colors) {
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
    NC <- length(colors);
    if (NC < 2) {
        stop (sprintf ("chmNewColorMap: colors vector contains %d color(s). It must contain at least two.", NC));
    }
    type = allowedTypes[mtype];
    if (class(values) == 'matrix') {
        if (type == "quantile") {
	    values = (1.0/(NC-1)) * (0:(NC-1));
	} else if (type == "linear") {
	    minv = median (apply (values, 1, function(v) min (v, na.rm=TRUE)), na.rm=TRUE);
	    maxv = median (apply (values, 1, function(v) max (v, na.rm=TRUE)), na.rm=TRUE);
	    if ((minv < 0.0) && (maxv > 0.0)) {
		maxv = max (maxv, -minv);
		minv = -maxv;
	    }
	    values = minv + (maxv - minv) * (1.0/(NC-1)) * (0:(NC-1));
	} else {
	    stop (sprintf ("chmNewColorMap: unable to derive color map breaks from matrix for map type '%s'", type));
	}
    } else if (any (class(values) == c('character', 'numeric', 'integer', 'logical'))) {
	if (length(values) != NC)
	    stop (sprintf ("chmNewColorMap: number of values (%d) does not equal number of color (%d). It should.", length(values), NC));
    } else {
        stop (sprintf ("chmNewColorMap: values vector has unknown class '%s'. It must be either a vector or a matrix.",
	               class (values)));
    }
    pts <- NULL;
    for (ii in 1:length(values))
        pts <- append (pts, new (Class="ngchmColorpt", value=values[ii], color=colors[ii]));
    new (Class="ngchmColormap", type=type, missing=missing, points=pts)
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
#' cloudServ <- chmServer ("dnsname.domain")
#'
#' @seealso ngchmServer-class
#' @seealso chmMake
#' @seealso chmInstall
#' @seealso chmUninstall
#'
chmServer <- function (serverName, username=NULL, keypath=NULL, serverPort=8080, deployServer=NULL, deployDir=NULL,
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
