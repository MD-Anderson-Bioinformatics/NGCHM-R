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
	                  data="matrix",
			  row.type="optCharacter",
			  column.type="optCharacter",
			  row.covariates="optList",
	                  column.covariates="optList"));

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
          representation (name="character", data="matrix", colors="ngchmColormap"));

#' Class representing a Generic Property for a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmProperty
#' @name ngchmProperty-class
#' @rdname ngchmProperty-class
#'
#' @keywords classes
setClass ("ngchmProperty",
          representation (label="character", value="character"));

#' Class representing a Menu Item for a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmMenuItem
#' @name ngchmMenuItem-class
#' @rdname ngchmMenuItem-class
#'
#' @keywords classes
setClass ("ngchmMenuItem",
          representation (label="character", description="character", fun="character"));

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

#' Class representing a type attached to an axis in a Next Generation Clustered Heat Map (NGCHM).
#'
#' @name ngchmAxisType-class
#' @rdname ngchmAxisType-class
#'
#' @keywords classes
setClass ("ngchmAxisType",
          representation (where="character", type="character", func="ngchmJS"));

#' Class representing an axis function for Next Generation Clustered Heat Map (NGCHM).
#'
#' @name ngchmAxisFunction-class
#' @rdname ngchmAxisFunction-class
#'
#' @keywords classes
setClass ("ngchmAxisFunction",
          representation (type="character", label="character", func="ngchmJS"));

#' Class representing a matrix function for Next Generation Clustered Heat Map (NGCHM).
#'
#' @name ngchmMatrixFunction-class
#' @rdname ngchmMatrixFunction-class
#'
#' @keywords classes
setClass ("ngchmMatrixFunction",
          representation (rowtype="character", columntype="character", label="character", func="ngchmJS"));

#' Class representing a type mapper function for Next Generation Clustered Heat Map (NGCHM).
#'
#' @name ngchmTypeMapper-class
#' @rdname ngchmTypeMapper-class
#'
#' @keywords classes
setClass ("ngchmTypeMapper",
          representation (fromtype="character", totype="character", func="ngchmJS"));

#' Class representing custom CSS for a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmCSS
#' @name ngchmCSS-class
#' @rdname ngchmCSS-class
#'
#' @keywords classes
setClass ("ngchmCSS",
          representation (css="character"));

#' Class representing a Classfication Bar on a Next Generation Clustered Heat Map (NGCHM).
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

#' Class representing an overview of a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmOverview
#' @name ngchmOverview-class
#' @rdname ngchmOverview-class
#'
#' @keywords classes
setClass ("ngchmOverview",
          representation (format="character",
			  width="integer",
			  height="integer"));

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
#' @seealso chmAddClassBar
#' @seealso chmAddDataset
#' @seealso chmAddMenuItem
#' @seealso chmAddCSS
#' @seealso chmMake
#' @seealso chmInstall
#' 
setClass ("ngchm",
          representation (name="character",
	                  inpDir="character",
			  outDir="character",
			  propFile="character",
			  layers="optList",
			  colormaps="optList",
			  rowMenu="optList",
			  colMenu="optList",
			  datasets="optList",
			  tags="optCharacter",
			  elementMenu="optList",
			  rowTypeFunctions="optList",   # Type functions specific to this CHM.
			  colTypeFunctions="optList",
			  elementTypeFunctions="optList",
			  axisTypes="optList",
			  css="optList",
			  extrafiles="optCharacter",
			  properties="optList",
			  overviews="optList",
			  javascript="optList",
			  rowOrder="optDendrogram",
			  colOrder="optDendrogram",
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
	                        inpDir="ngchm.input",
				outDir="ngchm.output",
				propFile="ngchm.input/chm.properties",
				layers=c(),
				colormaps=NULL,
				rowOrder=NULL,
				colOrder=NULL,
				rowMeta=NULL,
				colMeta=NULL,
				axisTypes=NULL,
				datasets=NULL,
				tags=c(),
				css=c(),
			        rowTypeFunctions=NULL,
			        colTypeFunctions=NULL,
			        elementTypeFunctions=NULL,
				extrafiles=c(),
				properties=c(),
				overviews=NULL,
				relatedLinks=NULL,
				relatedGroups=NULL,
				templates=NULL,
				width=as.integer(500),
				height=as.integer(500)));

#' Class representing a deployment method for a Next Generation Clustered Heat Map (NGCHM) server.
#'
#' @exportClass ngchmServerProtocol
#' @name ngchmServerProtocol-class
#' @rdname ngchmServerProtocol-class
#'
#' @keywords classes
setClass ("ngchmServerProtocol",
          representation (protocolName="character",
	                  installMethod="function", uninstallMethod="function",
	                  makePrivate="function", makePublic="function"));

#' Class representing a Next Generation Clustered Heat Map (NGCHM) server.
#'
#' @exportClass ngchmServer
#' @name ngchmServer-class
#' @rdname ngchmServer-class
#'
#' @keywords classes
setClass ("ngchmServer",
          representation (name="character", deployServer="optCharacter", deployDir="optCharacter", urlBase="character",
			  serverProtocol="ngchmServerProtocol", traceLevel="optCharacter",
	                  username="optCharacter", keypath="optCharacter", jarFile="character"));
