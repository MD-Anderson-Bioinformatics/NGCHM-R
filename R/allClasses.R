#' @import methods
setOldClass ("dendrogram");
setOldClass ("hclust");
setOldClass ("file");
setOldClass ("fileContent");

#' Optional Dendrogram
#'
## @exportClass optDendrogram
#' @name optDendrogram-class
#' @rdname optDendrogram-class
setClassUnion ("optDendrogram");
setIs ("dendrogram", "optDendrogram");
setIs ("character", "optDendrogram");
setIs ("hclust", "optDendrogram");
setIs ("file", "optDendrogram");
setIs ("fileContent", "optDendrogram");
setIs ("function", "optDendrogram");
setIs ("NULL", "optDendrogram");

setClassUnion ("optCharacter");
setIs ("character", "optCharacter");
setIs ("NULL", "optCharacter");

setClassUnion ("optNumeric");
setIs ("numeric", "optNumeric");
setIs ("NULL", "optNumeric");

setClassUnion ("optInteger");
setIs ("integer", "optInteger");
setIs ("NULL", "optInteger");

setClassUnion ("charOrFunction");
setIs ("character", "charOrFunction");
setIs ("function", "charOrFunction");

setClassUnion ("optList");
setIs ("list", "optList");
setIs ("NULL", "optList");

setClassUnion ("numericOrCharacter");
setIs ("numeric", "numericOrCharacter");
setIs ("character", "numericOrCharacter");
setIs ("logical", "numericOrCharacter");

s4ToList <- function(x,...) {
    c(list(class=class(x)),mapply(function(s)slot(x,s),slotNames(class(x)),SIMPLIFY=FALSE))
}
s4ToJSON <- function(x,...) {
    toJSON(s4ToList(x))
}

#' Class representing the shaid of an object
#'
#' @name shaid-class
#' @rdname shaid-class
#'
#' @keywords shaid
setClass ("shaid", slots=list(type='character', value="character"));

setMethod ('show',
           signature = c('shaid'),
           definition = function (object) {
	       cat (sprintf ("shaid %s %s\n", object@type, object@value));
	   });

setIs ("shaid", "optDendrogram");
setMethod(jsonlite:::asJSON, signature=c("shaid"), definition=s4ToJSON);

#' Class representing the properties of a data point in a Next Generation Clustered Heat Map (NGCHM).
#'
#' @name ngchmValueProp-class
#' @rdname ngchmValueProp-class
#'
#' @keywords classes
setClass ("ngchmValueProp",
          representation (value="numericOrCharacter",
	                  color="character",
	                  name="optCharacter",
	                  shape="optCharacter",
	                  z="optNumeric"
			  ));

setMethod ('show',
           signature = c('ngchmValueProp'),
	   definition = function (object) {
	       cat (sprintf ("ngchmValueProp(%d values)\n", length(object@value)));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmValueProp"), definition=s4ToJSON);

#' Class representing a Dataset attached to a NGCHM
#'
#' @exportClass ngchmDataset
#' @name ngchmDataset-class
#' @rdname ngchmDataset-class
#'
#' @keywords classes
setClass ("ngchmDataset",
          representation (name="character",
			  description="character",
	                  data="shaid",
			  row.type="optCharacter",
			  column.type="optCharacter",
			  row.covariates="optList",
	                  column.covariates="optList"));

setMethod ('show',
           signature = c('ngchmDataset'),
	   definition = function (object) {
	       cat (sprintf ("ngchmDataset '%s': %s (%s x %s)\n", object@name, object@data@value,
			     if (length(object@column.type)==0) "unknown" else object@column.type,
			     if (length(object@row.type)==0) "unknown" else object@row.type
			     ));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmDataset"), definition=s4ToJSON);

#' Class representing a Covariate attached to a Dataset
#'
#' @exportClass ngchmCovariate
#' @name ngchmCovariate-class
#' @rdname ngchmCovariate-class
#'
#' @keywords classes
setClass ("ngchmCovariate",
          representation (label="character",
			  type="character",
			  fullname="character",
			  label.series = "character",
	                  series.properties="optList"),
	  prototype = prototype(label=character(0),
	                        fullname=character(0),
				label.series=NULL,
				series.properties=NULL));

setMethod ('show',
           signature = c('ngchmCovariate'),
	   definition = function (object) {
	       cat (sprintf ("ngchmCovariate '%s' of type '%s'\n", object@label, object@type));
	   });

#' Class representing a Template attached to a NGCHM
#'
#' @exportClass ngchmTemplate
#' @name ngchmTemplate-class
#' @rdname ngchmTemplate-class
#'
#' @keywords classes
setClass ("ngchmTemplate",
          representation (source.path="charOrFunction",
			  dest.path="character",
	                  substitutions="optList"));

#' Class representing a link related to a NGCHM
#'
#' @exportClass ngchmRelated
#' @name ngchmRelated-class
#' @rdname ngchmRelated-class
#'
#' @keywords classes
setClass ("ngchmRelated",
          representation (group="character",
	                  link="character",
	                  description="character"));

#' Class representing a group of related links to a NGCHM
#'
#' @exportClass ngchmRelatedGroup
#' @name ngchmRelatedGroup-class
#' @rdname ngchmRelatedGroup-class
#'
#' @keywords classes
setClass ("ngchmRelatedGroup",
          representation (name="character",
	                  header="character",
	                  linktype="character",
	                  blurb="optCharacter"));

#' Class representing a Color Map on a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmColormap
#' @name ngchmColormap-class
#' @rdname ngchmColormap-class
#'
#' @keywords classes
setClass ("ngchmColormap",
          representation (type="character",
	                  missing="optCharacter",
	                  points="optList"));

setMethod ('show',
           signature = c('ngchmColormap'),
	   definition = function (object) {
	       cat (sprintf ("ngchmColormap of type '%s'\n", object@type));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmColormap"), definition=s4ToJSON);

setClassUnion ("optColormap");
setIs ("ngchmColormap", "optColormap");
setIs ("NULL", "optColormap");
setIs ("ngchmColormap", "optList");

#' Class representing a Layer on a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmLayer
#' @name ngchmLayer-class
#' @rdname ngchmLayer-class
#'
#' @keywords classes
setClass ("ngchmLayer",
          slots=list(name="character", data="shaid", colors="ngchmColormap", summarizationMethod="character"));

setMethod ('show',
           signature = c('ngchmLayer'),
	   definition = function (object) {
	       #cat (sprintf ("ngchmLayer '%s': %d x %d\n", object@name, ncol(object@data), nrow(object@data)));
	       cat (sprintf ("ngchmLayer '%s': %s\n", object@name, object@data@value));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmLayer"), definition=s4ToJSON);

#' Class representing a Generic Property for a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmProperty
#' @name ngchmProperty-class
#' @rdname ngchmProperty-class
#'
#' @keywords classes
setClass ("ngchmProperty",
          representation (label="character", value="character"));

setMethod ('show',
           signature = c('ngchmProperty'),
	   definition = function (object) {
	       cat (sprintf ("ngchmProperty '%s': '%s'\n", object@label, paste(object@value,collapse='///')));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmProperty"), definition=s4ToJSON);

#' Class representing a Menu Item for a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmMenuItem
#' @name ngchmMenuItem-class
#' @rdname ngchmMenuItem-class
#'
#' @keywords classes
setClass ("ngchmMenuItem",
          representation (label="character", description="character", fun="character"));

setMethod ('show',
           signature = c('ngchmMenuItem'),
	   definition = function (object) {
	       cat (sprintf ("ngchmMenuItem '%s': '%s'\n", object@label, object@description));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmMenuItem"), definition=s4ToJSON);

#' Class representing a custom Javascript function for a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmJS
#' @name ngchmJS-class
#' @rdname ngchmJS-class
#'
#' @keywords classes
setClass ("ngchmJS",
          representation (name="character", description="character", script="character", requires="optCharacter",
	                  extraParams="optCharacter", global="logical"));

setMethod ('show',
           signature = c('ngchmJS'),
	   definition = function (object) {
	       cat (sprintf ("ngchmJS '%s': '%s'\n", object@name, object@description));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmJS"), definition=s4ToJSON);

#' Class representing a type attached to an axis in a Next Generation Clustered Heat Map (NGCHM).
#'
#' @name ngchmAxisType-class
#' @rdname ngchmAxisType-class
#'
#' @keywords classes
setClass ("ngchmAxisType",
          representation (where="character", type="character", func="ngchmJS"));

setMethod ('show',
           signature = c('ngchmAxisType'),
	   definition = function (object) {
	       cat (sprintf ("ngchmAxisType %s=%s\n", object@where, object@type));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmAxisType"), definition=s4ToJSON);

#' Class representing an axis function for Next Generation Clustered Heat Map (NGCHM).
#'
#' @name ngchmAxisFunction-class
#' @rdname ngchmAxisFunction-class
#'
#' @keywords classes
setClass ("ngchmAxisFunction",
          representation (type="character", label="character", func="ngchmJS"));

setMethod ('show',
           signature = c('ngchmAxisFunction'),
	   definition = function (object) {
	       cat (sprintf ("ngchmAxisFunction %s -> %s\n", object@type, object@label));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmAxisFunction"), definition=s4ToJSON);

#' Class representing a matrix function for Next Generation Clustered Heat Map (NGCHM).
#'
#' @name ngchmMatrixFunction-class
#' @rdname ngchmMatrixFunction-class
#'
#' @keywords classes
setClass ("ngchmMatrixFunction",
          representation (rowtype="character", columntype="character", label="character", func="ngchmJS"));

setMethod ('show',
           signature = c('ngchmMatrixFunction'),
	   definition = function (object) {
	       cat (sprintf ("ngchmMatrixFunction %s x %s -> %s\n", object@rowtype, object@coltype, object@label));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmMatrixFunction"), definition=s4ToJSON);

#' Class representing a type mapper function for Next Generation Clustered Heat Map (NGCHM).
#'
#' @name ngchmTypeMapper-class
#' @rdname ngchmTypeMapper-class
#'
#' @keywords classes
setClass ("ngchmTypeMapper",
          representation (fromtype="character", totype="character", func="ngchmJS"));

setMethod ('show',
           signature = c('ngchmTypeMapper'),
	   definition = function (object) {
	       cat (sprintf ("ngchmTypeMapper %s -> %s\n", object@fromtype, object@totype));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmTypeMapper"), definition=s4ToJSON);

#' Class representing custom CSS for a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmCSS
#' @name ngchmCSS-class
#' @rdname ngchmCSS-class
#'
#' @keywords classes
setClass ("ngchmCSS",
          representation (css="character"));

setMethod ('show',
           signature = c('ngchmCSS'),
	   definition = function (object) {
	       cat (sprintf ("ngchmCSS %s\n", object@css));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmCSS"), definition=s4ToJSON);

#' Class representing a Covariate Bar on a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmBar
#' @name ngchmBar-class
#' @rdname ngchmBar-class
#'
#' @keywords classes
setClass ("ngchmBar",
          representation (type="character",
	                  label="character",
			  data="numericOrCharacter",
			  display="character",
			  merge="optCharacter",
			  thickness="integer",
			  axisTypes="optList",
			  colors="optColormap"));

setMethod ('show',
           signature = c('ngchmBar'),
	   definition = function (object) {
	       cat (sprintf ("ngchmBar %s\n", object@label));
	   });

#' Class representing an overview of a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmOverview
#' @name ngchmOverview-class
#' @rdname ngchmOverview-class
#'
#' @keywords classes
setClass ("ngchmOverview",
          representation (format="character",
			  width="optInteger",
			  height="optInteger"));

setMethod ('show',
           signature = c('ngchmOverview'),
	   definition = function (object) {
	       cat (sprintf ("ngchmOverview %s %d x %d\n", object@format,
	                     if (is.null(object@width)) 0 else object@width,
			     if (is.null(object@height)) 0 else object@height));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmOverview"), definition=s4ToJSON);

#' Class representing an addon dialog
#'
#' @exportClass ngchmDialog
#' @name ngchmDialog-class
#' @rdname ngchmDialog-class
#'
#' @keywords classes
setClass ("ngchmDialog",
          representation (id="character",
			  title="character",
			  fn="ngchmJS"));

setMethod ('show',
           signature = c('ngchmDialog'),
	   definition = function (object) {
	       cat (sprintf ("ngchmDialog %s: %s\n", object@id, object@title));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmDialog"), definition=s4ToJSON);

#' Class representing an axis of a Next Generation Clustered Heat Map (NG-CHM).
#'
#' @exportClass ngchmAxis
#' @name ngchmAxis-class
#' @rdname ngchmAxis-class
#' @keywords classes
#'
#' @seealso chmAxis
setClass ("ngchmAxis",
          representation (axis="character",
                          objects="optList"));

setMethod ('show',
           signature = c('ngchmAxis'),
           definition = function(object) {
	       cat (sprintf ("ngchmAxis %s (%d objects)\n", object@axis, length(object@objects)));
	   });

#' Class representing a Next Generation Clustered Heat Map (NGCHM) under construction.
#'
#' @exportClass ngchm
#' @name ngchm-class
#' @rdname ngchm-class
#'
#' @keywords classes
#'
#' @seealso chmNew
#' @seealso chmRowOrder
#' @seealso chmColOrder
#' @seealso chmAddLayer
#' @seealso chmAddCovariateBar
#' @seealso chmAddDataset
#' @seealso chmAddMenuItem
#' @seealso chmAddCSS
#' @seealso chmMake
#' @seealso chmInstall
#' 
setClassUnion ("ngchm");

setClass ("ngchmVersion1",
          representation (name="character",
			  version="integer",
			  uuid="character",
			  baggage="optCharacter",
	                  inpDir="character",
			  outDir="character",
			  saveDir="character",
			  propFile="character",
			  layers="optList",
			  colormaps="optList",
			  rowMenu="optList",
			  colMenu="optList",
			  datasets="optList",
			  dialogs="optList",
			  tags="optCharacter",
			  elementMenu="optList",
			  rowTypeFunctions="optList",   # Type functions specific to this CHM.
			  colTypeFunctions="optList",
			  elementTypeFunctions="optList",
			  axisTypes="optList",
			  css="optList",
			  extrafiles="optCharacter",
			  extrascripts="optCharacter",
			  properties="optList",
			  overviews="optList",
			  javascript="optList",
			  rowOrder="optDendrogram", rowDist="charOrFunction", rowAgglom="charOrFunction",
			  colOrder="optDendrogram", colDist="charOrFunction", colAgglom="charOrFunction",
			  rowMeta="optList",
			  colMeta="optList",
			  rowClassbars="optList",
			  colClassbars="optList",
			  relatedLinks="optList",
			  relatedGroups="optList",
			  templates="optList",
			  width="integer",
			  height="integer"),
	  prototype = prototype(name=character(0),
				version=as.integer(1),
				uuid="",
				baggage=NULL,
	                        inpDir="",
				outDir="",
				saveDir=".",
				propFile="chm.properties",
				layers=c(),
				colormaps=NULL,
				rowOrder=NULL, rowDist="correlation", rowAgglom="ward",
				colOrder=NULL, colDist="correlation", colAgglom="ward",
				rowMeta=NULL,
				colMeta=NULL,
				axisTypes=NULL,
				datasets=NULL,
				dialogs=NULL,
				tags=c(),
				css=c(),
			        rowTypeFunctions=NULL,
			        colTypeFunctions=NULL,
			        elementTypeFunctions=NULL,
				extrafiles=c(),
				extrascripts=c(),
				properties=c(),
				overviews=NULL,
				relatedLinks=NULL,
				relatedGroups=NULL,
				templates=NULL,
				width=as.integer(500),
				height=as.integer(500)));
setIs ("ngchmVersion1", "ngchm");

setClass ("ngchmVersion2",
          representation (name="character",
			  version="integer",
                          format="character",
			  uuid="character",
			  baggage="optCharacter",
	                  inpDir="character",
			  outDir="character",
			  saveDir="character",
			  propFile="character",
			  layers="optList",
			  colormaps="optList",
			  rowMenu="optList",
			  colMenu="optList",
			  datasets="optList",
			  dialogs="optList",
			  tags="optCharacter",
			  elementMenu="optList",
			  rowTypeFunctions="optList",   # Type functions specific to this CHM.
			  colTypeFunctions="optList",
			  elementTypeFunctions="optList",
			  axisTypes="optList",
			  css="optList",
			  extrafiles="optCharacter",
			  extrascripts="optCharacter",
			  properties="optList",
			  overviews="optList",
			  javascript="optList",
			  rowOrder="optDendrogram", rowDist="charOrFunction", rowAgglom="charOrFunction",
			  colOrder="optDendrogram", colDist="charOrFunction", colAgglom="charOrFunction",
			  rowMeta="optList",
			  colMeta="optList",
			  rowCovariateBars="optList",
			  colCovariateBars="optList",
			  relatedLinks="optList",
			  relatedGroups="optList",
			  templates="optList",
			  width="integer",
			  height="integer"),
	  prototype = prototype(name=character(0),
				version=as.integer(2),
                                format="original",
				uuid="",
				baggage=NULL,
	                        inpDir="",
				outDir="",
				saveDir=".",
				propFile="chm.properties",
				layers=c(),
				colormaps=NULL,
				rowOrder=NULL, rowDist="correlation", rowAgglom="ward",
				colOrder=NULL, colDist="correlation", colAgglom="ward",
				rowMeta=NULL,
				colMeta=NULL,
				axisTypes=NULL,
				datasets=NULL,
				dialogs=NULL,
				tags=c(),
				css=c(),
			        rowTypeFunctions=NULL,
			        colTypeFunctions=NULL,
			        elementTypeFunctions=NULL,
				extrafiles=c(),
				extrascripts=c(),
				properties=c(),
				overviews=NULL,
				relatedLinks=NULL,
				relatedGroups=NULL,
				templates=NULL,
				width=as.integer(500),
				height=as.integer(500)));
setIs ("ngchmVersion2", "ngchm");

setMethod ('show',
           signature = c('ngchm'),
	   definition = function (object) {
	       cat (sprintf ("ngchm %s (%d layers)\n", object@name, length(object@layers)));
	   });
setMethod(jsonlite:::asJSON, signature=c("ngchmVersion2"), definition=function(x,...) {
    l <- s4ToList(x);
    l$class <- "ngchm";
    l <- prepChmOrderings (x, l);
    l$layers <- mapply (function(layer)prepDataLayer(x,layer), x@layers, SIMPLIFY=FALSE);
    empty <- vapply(l, function(x)length(x)==0, TRUE);
    exclude <- vapply(names(l), function(x)x %in% c("width","height","uuid","baggage","inpDir","outDir","saveDir","propFile","css"), TRUE);
    toJSON(l[-which(empty|exclude)], pretty=TRUE)
});

#' Class representing a deployment method for a Next Generation Clustered Heat Map (NGCHM) server.
#'
#' @exportClass ngchmServerProtocol
#' @name ngchmServerProtocol-class
#' @rdname ngchmServerProtocol-class
#'
#' @keywords classes
setClass ("ngchmServerProtocol",
          representation (protocolName="character",
                          requiredParams="optCharacter",
                          optionalParams="optCharacter",
			  paramValidator="function",
	                  installMethod="function", uninstallMethod="function",
	                  makePrivate="function", makePublic="function"));

setMethod ('show',
           signature = c('ngchmServerProtocol'),
	   definition = function (object) {
	       cat (sprintf ("ngchmServerProtocol %s\n", object@protocolName));
	   });

#' Class representing a Next Generation Clustered Heat Map (NGCHM) server.
#'
#' @exportClass ngchmServer
#' @name ngchmServer-class
#' @rdname ngchmServer-class
#'
#' @keywords classes
setClass ("ngchmServer",
          representation (name="character",
                          serverURL="optCharacter",
                          traceLevel="optCharacter",
                          jarFile="optCharacter",
			  serverProtocol="ngchmServerProtocol",
                          deployServer="optCharacter",
                          viewServer="optCharacter",
                          protoOpts="optList"			# Protocol-specific parameters
                          ));

setMethod ('show',
           signature = c('ngchmServer'),
	   definition = function (object) {
	       cat (sprintf ("ngchmServer %s\n", object@name));
	   });
