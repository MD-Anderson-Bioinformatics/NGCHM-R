#' @import methods
setOldClass("dendrogram")
setOldClass("hclust")
setOldClass("file")
setOldClass("fileContent")
setOldClass("singleElement")
#' Optional Dendrogram
#'
## @exportClass optDendrogram
#' @name optDendrogram-class
#' @rdname optDendrogram-class
setClassUnion("optDendrogram")
setIs("dendrogram", "optDendrogram")
setIs("character", "optDendrogram")
setIs("hclust", "optDendrogram")
setIs("file", "optDendrogram")
setIs("fileContent", "optDendrogram")
setIs("function", "optDendrogram")
setIs("NULL", "optDendrogram")
setClassUnion("optCharacter")
setIs("character", "optCharacter")
setIs("NULL", "optCharacter")
setClassUnion("optNumeric")
setIs("numeric", "optNumeric")
setIs("NULL", "optNumeric")
setClassUnion("optInteger")
setIs("integer", "optInteger")
setIs("NULL", "optInteger")
setClassUnion("charOrFunction")
setIs("character", "charOrFunction")
setIs("function", "charOrFunction")
setClassUnion("optList")
setIs("list", "optList")
setIs("NULL", "optList")
setClassUnion("numericOrCharacter")
setIs("numeric", "numericOrCharacter")
setIs("character", "numericOrCharacter")
setIs("logical", "numericOrCharacter")
s4ToList <- function(x, ...) {
  c(list(class = class(x)), mapply(function(s) slot(x, s), slotNames(class(x)), SIMPLIFY = FALSE))
}
listFix <- function(l, single, exclude, extra, rename) {
  if (missing(single)) single <- NULL
  if (missing(exclude)) exclude <- NULL
  if (missing(extra)) extra <- NULL
  if (missing(rename)) rename <- NULL
  # Add/replace elements in extra to l
  if (length(extra) > 0) {
    for (ii in 1:length(extra)) {
      l[[names(extra)[ii]]] <- extra[[ii]]
    }
  }
  # Rename elements in list rename: names are new names, values are old names.
  for (nn in names(rename)) {
    on <- rename[[nn]]
    if (!identical(l[[on]], NULL)) {
      l[[nn]] <- l[[on]]
      l[[on]] <- NULL
    }
  }
  # Set class of single elements
  for (elem in single) {
    if (!identical(l[[elem]], NULL)) {
      class(l[[elem]]) <- "singleElement"
    }
  }
  # Remove empty elements and members of exclude
  exclude <- vapply(names(l), function(x) x %in% exclude, TRUE)
  empty <- vapply(l, function(x) length(x) == 0, TRUE)
  if (any(empty | exclude)) l <- l[-which(empty | exclude)]
  l
}
s4ToJSON <- function(x, ...) {
  toJSON(s4ToList(x))
}

#' Class representing the shaid of an object
#'
#' @name shaid-class
#' @rdname shaid-class
#'
#' @keywords shaid
setClass("shaid", slots = list(type = "character", value = "character"))
setMethod("show",
  signature = c("shaid"),
  definition = function(object) {
    cat(sprintf("shaid %s %s\n", object@type, object@value))
  }
)
setIs("shaid", "optDendrogram")
setMethod(jsonlite:::asJSON, signature = c("shaid"), definition = function(x, ...) {
  paste0('{ "class": "shaid", "type": "', x@type, '", "value": "', x@value, '" }')
})
#' Class representing the properties of a data point in a Next Generation Clustered Heat Map (NGCHM).
#'
#' @name ngchmValueProp-class
#' @rdname ngchmValueProp-class
#'
#' @keywords classes
setClass(
  "ngchmValueProp",
  representation(
    value = "numericOrCharacter",
    color = "character",
    name = "optCharacter",
    shape = "optCharacter",
    z = "optNumeric"
  )
)
setMethod("show",
  signature = c("ngchmValueProp"),
  definition = function(object) {
    cat(sprintf("ngchmValueProp(%d values)\n", length(object@value)))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmValueProp"), definition = function(x, ...) {
  l <- s4ToList(x)
  singleElements <- c("class", "value", "color", "name", "shape", "z")
  for (elem in singleElements) {
    if (!identical(l[[elem]], NULL)) {
      class(l[[elem]]) <- "singleElement"
    }
  }
  empty <- vapply(l, function(x) length(x) == 0, TRUE)
  if (any(empty)) l <- l[-which(empty)]
  toJSON(l)
})
#' Class representing a Dataset attached to a NGCHM
#'
#' @exportClass ngchmDataset
#' @name ngchmDataset-class
#' @rdname ngchmDataset-class
#'
#' @keywords classes
setClass(
  "ngchmDataset",
  representation(
    name = "character",
    description = "character",
    data = "shaid",
    row.type = "optCharacter",
    column.type = "optCharacter",
    row.covariates = "optList",
    column.covariates = "optList"
  )
)
setMethod("show",
  signature = c("ngchmDataset"),
  definition = function(object) {
    cat(sprintf(
      "ngchmDataset '%s': %s (%s x %s)\n", object@name, object@data@value,
      if (length(object@column.type) == 0) "unknown" else object@column.type,
      if (length(object@row.type) == 0) "unknown" else object@row.type
    ))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmDataset"), definition = function(x, ...) {
  l <- s4ToList(x)
  l <- listFix(l, single = c("class", "name", "description", "row.type", "column.type"))
  toJSON(l)
})
#' Class representing a Covariate attached to a Dataset
#'
#' @exportClass ngchmCovariate
#' @name ngchmCovariate-class
#' @rdname ngchmCovariate-class
#'
#' @keywords classes
setClass(
  "ngchmCovariate",
  representation(
    label = "character",
    type = "character",
    fullname = "character",
    label.series = "shaid",
    series.properties = "optList"
  )
)
setMethod("show",
  signature = c("ngchmCovariate"),
  definition = function(object) {
    cat(sprintf("ngchmCovariate '%s' of type '%s'\n", object@label, object@type))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmCovariate"), definition = function(x, ...) {
  l <- s4ToList(x)
  singleElements <- c("class", "label", "type", "fullname")
  l$data <- l$label.series
  idx <- which(vapply(ngchm.env$covariateRenderers, function(x) sameColormap(x, l$series.properties), TRUE))
  if (length(idx) == 1) {
    l$renderer <- idx - 1
    singleElements <- c(singleElements, "renderer")
  }
  l <- listFix(l, single = singleElements, exclude = c("label.series", "series.properties"))
  toJSON(l)
})
#' Class representing a Template attached to a NGCHM
#'
#' @exportClass ngchmTemplate
#' @name ngchmTemplate-class
#' @rdname ngchmTemplate-class
#'
#' @keywords classes
setClass(
  "ngchmTemplate",
  representation(
    source.path = "charOrFunction",
    dest.blob = "shaid",
    dest.path = "character",
    substitutions = "optList"
  )
)
setMethod(jsonlite:::asJSON, signature = c("ngchmTemplate"), definition = function(x, ...) {
  l <- s4ToList(x)
  l <- listFix(l, single = c("class", "dest.path"), exclude = c("source.path", "substitutions"))
  toJSON(l)
})
#' Class representing meta data attached to an NG-CHM
#'
#' @exportClass ngchmMetaData
#' @name ngchmMetaData-class
#' @rdname ngchmMetaData-class
#'
#' @keywords classes
setClass(
  "ngchmMetaData",
  representation(type = "character", value = "shaid")
)
setMethod(jsonlite:::asJSON, signature = c("ngchmMetaData"), definition = function(x, ...) {
  l <- s4ToList(x)
  l <- listFix(l, single = c("class"))
  toJSON(l)
})
#' Class representing a link related to a NGCHM
#'
#' @exportClass ngchmRelated
#' @name ngchmRelated-class
#' @rdname ngchmRelated-class
#'
#' @keywords classes
setClass(
  "ngchmRelated",
  representation(
    group = "character",
    link = "character",
    description = "character"
  )
)
setMethod(jsonlite:::asJSON, signature = c("ngchmRelated"), definition = function(x, ...) {
  l <- s4ToList(x)
  l <- listFix(l, single = c("class", "group", "link", "description"))
  toJSON(l)
})
#' Class representing a group of related links to a NGCHM
#'
#' @exportClass ngchmRelatedGroup
#' @name ngchmRelatedGroup-class
#' @rdname ngchmRelatedGroup-class
#'
#' @keywords classes
setClass(
  "ngchmRelatedGroup",
  representation(
    name = "character",
    header = "character",
    linktype = "character",
    blurb = "optCharacter"
  )
)
setMethod(jsonlite:::asJSON, signature = c("ngchmRelatedGroup"), definition = function(x, ...) {
  l <- s4ToList(x)
  l <- listFix(l, single = c("class", "name", "header", "linktype", "blurb"), exclude = "fn")
  toJSON(l)
})
#' Class representing a Color Map on a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmColormap
#' @name ngchmColormap-class
#' @rdname ngchmColormap-class
#'
#' @keywords classes
setClass(
  "ngchmColormap",
  representation(
    type = "character",
    missing = "optCharacter",
    points = "optList"
  )
)
setMethod("show",
  signature = c("ngchmColormap"),
  definition = function(object) {
    cat(sprintf("ngchmColormap of type '%s'\n", object@type))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmColormap"), definition = function(x, ...) {
  l <- s4ToList(x)
  singleElements <- c("class", "type", "missing")
  for (elem in singleElements) {
    if (!identical(l[[elem]], NULL)) {
      class(l[[elem]]) <- "singleElement"
    }
  }
  empty <- vapply(l, function(x) length(x) == 0, TRUE)
  if (any(empty)) l <- l[-which(empty)]
  toJSON(l)
})
setClassUnion("optColormap")
setIs("ngchmColormap", "optColormap")
setIs("NULL", "optColormap")
setIs("ngchmColormap", "optList")
#' Class representing a Layer on a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmLayer
#' @name ngchmLayer-class
#' @rdname ngchmLayer-class
#'
#' @keywords classes
setClass("ngchmLayer",
  slots = list(name = "character", data = "shaid", colors = "ngchmColormap", summarizationMethod = "character", cuts_color = "character")
)
setMethod("show",
  signature = c("ngchmLayer"),
  definition = function(object) {
    # cat (sprintf ("ngchmLayer '%s': %d x %d\n", object@name, ncol(object@data), nrow(object@data)));
    cat(sprintf("ngchmLayer '%s': %s\n", object@name, object@data@value))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmLayer"), definition = s4ToJSON)
#' Class representing a Generic Property for a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmProperty
#' @name ngchmProperty-class
#' @rdname ngchmProperty-class
#'
#' @keywords classes
setClass(
  "ngchmProperty",
  representation(label = "character", value = "character")
)
setMethod("show",
  signature = c("ngchmProperty"),
  definition = function(object) {
    cat(sprintf("ngchmProperty '%s': '%s'\n", object@label, paste(object@value, collapse = "///")))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmProperty"), definition = function(x, ...) {
  l <- s4ToList(x)
  l <- listFix(l, single = c("class", "label", "value"))
  toJSON(l)
})
#' Class representing a Menu Item for a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmMenuItem
#' @name ngchmMenuItem-class
#' @rdname ngchmMenuItem-class
#'
#' @keywords classes
setClass(
  "ngchmMenuItem",
  representation(label = "character", description = "character", fun = "character")
)
setMethod("show",
  signature = c("ngchmMenuItem"),
  definition = function(object) {
    cat(sprintf("ngchmMenuItem '%s': '%s'\n", object@label, object@description))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmMenuItem"), definition = s4ToJSON)
#' Class representing a custom Javascript function for a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmJS
#' @name ngchmJS-class
#' @rdname ngchmJS-class
#'
#' @keywords classes
setClass(
  "ngchmJS",
  representation(
    name = "character", description = "character", script = "character", requires = "optCharacter",
    extraParams = "optCharacter", global = "logical"
  )
)
setMethod("show",
  signature = c("ngchmJS"),
  definition = function(object) {
    cat(sprintf("ngchmJS '%s': '%s'\n", object@name, object@description))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmJS"), definition = s4ToJSON)
#' Class representing a type attached to an axis in a Next Generation Clustered Heat Map (NGCHM).
#'
#' @name ngchmAxisType-class
#' @rdname ngchmAxisType-class
#'
#' @keywords classes
setClass(
  "ngchmAxisType",
  representation(where = "character", type = "character", func = "ngchmJS")
)
setMethod("show",
  signature = c("ngchmAxisType"),
  definition = function(object) {
    cat(sprintf("ngchmAxisType %s=%s\n", object@where, object@type))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmAxisType"), definition = function(x, ...) {
  l <- s4ToList(x)
  l <- listFix(l, single = c("class", "where", "type"), exclude = "func")
  toJSON(l)
})
#' Class representing an axis function for Next Generation Clustered Heat Map (NGCHM).
#'
#' @name ngchmAxisFunction-class
#' @rdname ngchmAxisFunction-class
#'
#' @keywords classes
setClass(
  "ngchmAxisFunction",
  representation(type = "character", label = "character", func = "ngchmJS")
)
setMethod("show",
  signature = c("ngchmAxisFunction"),
  definition = function(object) {
    cat(sprintf("ngchmAxisFunction %s -> %s\n", object@type, object@label))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmAxisFunction"), definition = s4ToJSON)
#' Class representing a matrix function for Next Generation Clustered Heat Map (NGCHM).
#'
#' @name ngchmMatrixFunction-class
#' @rdname ngchmMatrixFunction-class
#'
#' @keywords classes
setClass(
  "ngchmMatrixFunction",
  representation(rowtype = "character", columntype = "character", label = "character", func = "ngchmJS")
)
setMethod("show",
  signature = c("ngchmMatrixFunction"),
  definition = function(object) {
    cat(sprintf("ngchmMatrixFunction %s x %s -> %s\n", object@rowtype, object@coltype, object@label))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmMatrixFunction"), definition = s4ToJSON)
#' Class representing a type mapper function for Next Generation Clustered Heat Map (NGCHM).
#'
#' @name ngchmTypeMapper-class
#' @rdname ngchmTypeMapper-class
#'
#' @keywords classes
setClass(
  "ngchmTypeMapper",
  representation(
    fromtype = "character", totype = "character",
    op = "character", params = "optList"
  )
)
setMethod("show",
  signature = c("ngchmTypeMapper"),
  definition = function(object) {
    cat(sprintf("ngchmTypeMapper %s -> %s\n", object@fromtype, object@totype))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmTypeMapper"), definition = function(x, ...) {
  l <- s4ToList(x)
  single <- c("class", "op", "totype")
  if (x@op == "field") {
    l$separator <- x@params$separator
    l$num <- x@params$num
    single <- c(single, "separator", "num")
  }
  if (x@op == "expr") {
    l$expr <- x@params$expr
    l$return <- x@params$return
    single <- c(single, "expr", "return")
  }
  l <- listFix(l, single = single, exclude = "params")
  toJSON(l)
})
#' Class representing custom CSS for a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmCSS
#' @name ngchmCSS-class
#' @rdname ngchmCSS-class
#'
#' @keywords classes
setClass(
  "ngchmCSS",
  representation(css = "character")
)
setMethod("show",
  signature = c("ngchmCSS"),
  definition = function(object) {
    cat(sprintf("ngchmCSS %s\n", object@css))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmCSS"), definition = s4ToJSON)
#' Class representing a Covariate Bar on a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmBar
#' @name ngchmBar-class
#' @rdname ngchmBar-class
#'
#' @keywords classes
setClass(
  "ngchmBar",
  representation(
    type = "character",
    label = "character",
    data = "shaid",
    display = "character",
    merge = "optCharacter",
    thickness = "integer",
    axisTypes = "optList",
    barType = "character",
    colors = "optColormap",
    loBound = "optNumeric", # For barType = barplot, scatterplot
    hiBound = "optNumeric", # same
    fgColor = "optCharacter", # same
    bgColor = "optCharacter" # same
  )
)
setMethod("show",
  signature = c("ngchmBar"),
  definition = function(object) {
    cat(sprintf("ngchmBar %s\n", object@label))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmBar"), definition = function(x, ...) {
  l <- s4ToList(x)
  rename <- list(bar_type = "barType", fg_color = "fgColor", bg_color = "bgColor", low_bound = "loBound", high_bound = "hiBound")
  singleElements <- c("class", "type", "label", "display", "merge", "thickness", names(rename))
  idx <- which(vapply(ngchm.env$covariateRenderers, function(x) sameColormap(x, l$colors), TRUE))
  if (length(idx) == 1) {
    l$renderer <- idx - 1
    singleElements <- c(singleElements, "renderer")
  }
  l <- listFix(l, rename = rename, single = singleElements, exclude = "colors")
  toJSON(l)
})
#' Class representing an overview of a Next Generation Clustered Heat Map (NGCHM).
#'
#' @exportClass ngchmOverview
#' @name ngchmOverview-class
#' @rdname ngchmOverview-class
#'
#' @keywords classes
setClass(
  "ngchmOverview",
  representation(
    format = "character",
    width = "optInteger",
    height = "optInteger"
  )
)
setMethod("show",
  signature = c("ngchmOverview"),
  definition = function(object) {
    cat(sprintf(
      "ngchmOverview %s %d x %d\n", object@format,
      if (is.null(object@width)) 0 else object@width,
      if (is.null(object@height)) 0 else object@height
    ))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmOverview"), definition = s4ToJSON)
#' Class representing an addon dialog
#'
#' @exportClass ngchmDialog
#' @name ngchmDialog-class
#' @rdname ngchmDialog-class
#'
#' @keywords classes
setClass(
  "ngchmDialog",
  representation(
    id = "character",
    title = "character",
    fn = "ngchmJS"
  )
)
setMethod("show",
  signature = c("ngchmDialog"),
  definition = function(object) {
    cat(sprintf("ngchmDialog %s: %s\n", object@id, object@title))
  }
)
setMethod(jsonlite:::asJSON, signature = c("ngchmDialog"), definition = function(x, ...) {
  l <- s4ToList(x)
  l <- listFix(l, single = c("class", "id", "title"), exclude = "fn")
  toJSON(l)
})
#' Class representing an axis of a Next Generation Clustered Heat Map (NG-CHM).
#'
#' @exportClass ngchmAxis
#' @name ngchmAxis-class
#' @rdname ngchmAxis-class
#' @keywords classes
#'
#' @seealso [chmAxis()]
setClass(
  "ngchmAxis",
  representation(
    axis = "character",
    objects = "optList"
  )
)
setMethod("show",
  signature = c("ngchmAxis"),
  definition = function(object) {
    cat(sprintf("ngchmAxis %s (%d objects)\n", object@axis, length(object@objects)))
  }
)

#' Class representing a Next Generation Clustered Heat Map (NGCHM) under construction.
#'
#' An NG-CHM is produced by creating a heat map object with [chmNew()], possibly modifying or augmenting it
#' using additional functions, such as [chmAddLayer()], [chmAddCovariateBar()], etc., and then
#' either saving it to a server using [chmInstall()] or saving it to a standalone file using [chmExportToFile()].
#'
#' @exportClass ngchm
#' @name ngchm-class
#' @rdname ngchm-class
#'
#' @keywords classes
#'
#' @seealso [chmNew()]
#' @seealso [chmRowOrder<-()]
#' @seealso [chmColOrder<-()]
#' @seealso [chmAdd()]
#' @seealso [chmAddLayer()]
#' @seealso [chmAddCovariateBar()]
#' @seealso [chmAddDataset()]
#' @seealso [chmAddAxisType()]
#' @seealso [chmInstall()]
#' @seealso [chmExportToFile()]
#'
setClassUnion("ngchm")
setClass("ngchmVersion1",
  representation(
    name = "character",
    version = "integer",
    uuid = "character",
    baggage = "optCharacter",
    inpDir = "character",
    outDir = "character",
    saveDir = "character",
    propFile = "character",
    layers = "optList",
    colormaps = "optList",
    rowMenu = "optList",
    colMenu = "optList",
    datasets = "optList",
    dialogs = "optList",
    tags = "optCharacter",
    elementMenu = "optList",
    rowTypeFunctions = "optList", # Type functions specific to this CHM.
    colTypeFunctions = "optList",
    elementTypeFunctions = "optList",
    axisTypes = "optList",
    css = "optList",
    extrafiles = "optCharacter",
    extrascripts = "optCharacter",
    properties = "optList",
    overviews = "optList",
    javascript = "optList",
    rowOrder = "optDendrogram", rowDist = "charOrFunction", rowAgglom = "charOrFunction",
    colOrder = "optDendrogram", colDist = "charOrFunction", colAgglom = "charOrFunction",
    rowMeta = "optList",
    colMeta = "optList",
    rowClassbars = "optList",
    colClassbars = "optList",
    relatedLinks = "optList",
    relatedGroups = "optList",
    templates = "optList",
    width = "integer",
    height = "integer"
  ),
  prototype = prototype(
    name = character(0),
    version = as.integer(1),
    uuid = "",
    baggage = NULL,
    inpDir = tempdir(),
    outDir = tempdir(),
    saveDir = tempdir(),
    propFile = "chm.properties",
    layers = c(),
    colormaps = NULL,
    rowOrder = NULL, rowDist = "correlation", rowAgglom = "ward",
    colOrder = NULL, colDist = "correlation", colAgglom = "ward",
    rowMeta = NULL,
    colMeta = NULL,
    axisTypes = NULL,
    datasets = NULL,
    dialogs = NULL,
    tags = c(),
    css = c(),
    rowTypeFunctions = NULL,
    colTypeFunctions = NULL,
    elementTypeFunctions = NULL,
    extrafiles = c(),
    extrascripts = c(),
    properties = c(),
    overviews = NULL,
    relatedLinks = NULL,
    relatedGroups = NULL,
    templates = NULL,
    width = as.integer(500),
    height = as.integer(500)
  )
)
setIs("ngchmVersion1", "ngchm")
#' Class representing ngchmVersion2 object
#'
#' @name ngchmVersion2-class
#' @rdname ngchmVersion2-class
#' @include panelClasses.R
#' @slot name The name under which the NGCHM will be saved to the NGCHM server.
#' @slot version Integer version number (default: 2)
#' @slot format (default: "original")
#' @slot uuid character
#' @slot baggage optCharacter
#' @slot inpDir character
#' @slot outDir character
#' @slot saveDir (default: tempdir())
#' @slot propFile (default: "chm.properties")
#' @slot layers List of data layers
#' @slot colormaps Color map
#' @slot rowMenu optList
#' @slot colMenu optList
#' @slot datasets optList
#' @slot dialogs optList
#' @slot tags optCharacter
#' @slot elementMenu optList
#' @slot rowTypeFunctions optList
#' @slot colTypeFunctions optList
#' @slot elementTypeFunctions optList
#' @slot axisTypes optList
#' @slot css optList
#' @slot extrafiles optCharacter
#' @slot extrascripts optCharacter
#' @slot properties optList
#' @slot overviews optList
#' @slot javascript optList
#' @slot rowOrder A vector, dendrogram, or function specifying the CHM row order
#' @slot rowDist Distance method to use by default RowOrder. (default: "correlation", which is 1 minus the Pearson correlation among the rows.)
#' @slot rowAgglom Agglomeration method to use by default RowOrder. Choices are those from stats::hclust. (default: "ward.D2")
#' @slot colOrder A vector, dendrogram, or function specifying the CHM column order.
#' @slot colDist Distance method to use by default ColOrder. (default: "correlation", which is 1 minus the Pearson correlation among the cols.)
#' @slot colAgglom Agglomeration method to use by default ColOrder. Choices are those from stats::hclust. (default: "ward.D2")
#' @slot rowOrderMethod character (default: "User")
#' @slot colOrderMethod character (default: "User")
#' @slot rowCutLocations Explicit list of row cut locations. If specified, rowTreeCuts is set to NULL.
#' @slot rowTreeCuts Number of tree cuts for row. If specified, rowCutLocations is set to NULL.
#' @slot rowCutWidth Width of row cuts (default: 5 rows)
#' @slot rowTopItems optCharacter
#' @slot rowDisplayLength optInteger
#' @slot rowDisplayAbbreviation optCharacter
#' @slot colCutLocations Explicit list of col cut locations. If specified, colTreeCuts is set to NULL.
#' @slot colTreeCuts Number of tree cuts for col. If specified, colCutLocations is set to NULL.
#' @slot colCutWidth Width of col cuts (defautl: 5 columns)
#' @slot colTopItems optCharacter
#' @slot colDisplayLength optInteger
#' @slot colDisplayAbbreviation optCharacter
#' @slot rowMeta optList
#' @slot colMeta optList
#' @slot rowCovariateBars optList
#' @slot colCovariateBars optList
#' @slot relatedLinks optList
#' @slot relatedGroups optList
#' @slot templates optList
#' @slot width default: 500
#' @slot height default: 500
#' @slot panel_configuration panel_configuration
setClass(
  Class = "ngchmVersion2",
  slots = list(
    name = "character",
    version = "integer",
    format = "character",
    uuid = "character",
    baggage = "optCharacter",
    inpDir = "character",
    outDir = "character",
    saveDir = "character",
    propFile = "character",
    layers = "optList",
    colormaps = "optList",
    rowMenu = "optList",
    colMenu = "optList",
    datasets = "optList",
    dialogs = "optList",
    tags = "optCharacter",
    elementMenu = "optList",
    rowTypeFunctions = "optList", # Type functions specific to this CHM.
    colTypeFunctions = "optList",
    elementTypeFunctions = "optList",
    axisTypes = "optList",
    css = "optList",
    extrafiles = "optCharacter",
    extrascripts = "optCharacter",
    properties = "optList",
    overviews = "optList",
    javascript = "optList",
    rowOrder = "optDendrogram", rowDist = "charOrFunction", rowAgglom = "charOrFunction",
    colOrder = "optDendrogram", colDist = "charOrFunction", colAgglom = "charOrFunction",
    rowOrderMethod = "character", colOrderMethod = "character",
    rowCutLocations = "optInteger", rowCutWidth = "optInteger", rowTreeCuts = "optInteger",
    rowTopItems = "optCharacter", rowDisplayLength = "optInteger", rowDisplayAbbreviation = "optCharacter",
    colCutLocations = "optInteger", colCutWidth = "optInteger", colTreeCuts = "optInteger",
    colTopItems = "optCharacter", colDisplayLength = "optInteger", colDisplayAbbreviation = "optCharacter",
    rowMeta = "optList",
    colMeta = "optList",
    rowCovariateBars = "optList",
    colCovariateBars = "optList",
    relatedLinks = "optList",
    relatedGroups = "optList",
    templates = "optList",
    width = "integer",
    height = "integer",
    panel_configuration = "panel_configuration"
  ),
)
setMethod(
  "initialize", "ngchmVersion2",
  function(.Object, name, version, format, baggage, inpDir, outDir, saveDir, propFile,
           layers, colormaps, rowOrder, rowDist, rowAgglom, colOrder, colDist, colAgglom, rowOrderMethod, colOrderMethod,
           rowCutLocations, rowCutWidth, rowTopItems, rowDisplayLength, rowDisplayAbbreviation,
           colCutLocations, colCutWidth, colTopItems, colDisplayLength, colDisplayAbbreviation,
           rowMeta, colMeta, axisTypes, datasets, dialogs, tags, css,
           rowTypeFunctions, colTypeFunctions, elementTypeFunctions, extrafiles,
           extrascripts, properties, overviews, relatedLinks, relatedGroups,
           templates, width, height, panel_configuration) {
    if (!missing(name)) {
      if (typeof(name) != "character") {
        stop(sprintf("Parameter 'name' must have type 'character', not '%s'", typeof(name)))
      }
      if (length(name) != 1) {
        stop(sprintf("Parameter 'name' must have a single value, not %d", length(name)))
      }
      if (nchar(name) == 0) {
        stop("Parameter 'name' cannot be the empty string")
      }
      .Object@name <- name
    } else {
      .Object@name <- "ngchm"
    }
    if (missing(version)) {
      .Object@version <- as.integer(2)
    } else {
      .Object@version <- as.integer(version)
    }
    .Object@uuid <- getuuid(.Object@name)
    if (!missing(format)) {
      .Object@format <- format
    } else {
      .Object@format <- "original"
    }
    if (!missing(baggage)) {
      .Object@baggage <- baggage
    } else {
      .Object@baggage <- NULL
    }
    if (!missing(inpDir)) {
      .Object@inpDir <- inpDir
    } else {
      .Object@inpDir <- tempdir()
    }
    if (!missing(outDir)) {
      .Object@outDir <- outDir
    } else {
      .Object@outDir <- tempdir()
    }
    if (!missing(saveDir)) {
      .Object@saveDir <- saveDir
    } else {
      .Object@saveDir <- tempdir()
    }
    if (!missing(propFile)) {
      .Object@propFile <- propFile
    } else {
      .Object@propFile <- "chm.properties"
    }
    if (!missing(layers)) {
      .Object@layers <- layers
    } else {
      .Object@layers <- c()
    }
    if (!missing(colormaps)) {
      .Object@colormaps <- colormaps
    } else {
      .Object@colormaps <- NULL
    }
    if (!missing(rowOrder)) {
      .Object@rowOrder <- rowOrder
    } else {
      .Object@rowOrder <- NULL
    }
    if (!missing(rowDist)) {
      .Object@rowDist <- rowDist
    } else {
      .Object@rowDist <- "correlation"
    }
    if (!missing(rowAgglom)) {
      .Object@rowAgglom <- rowAgglom
    } else {
      .Object@rowAgglom <- "ward.D2"
    }
    if (!missing(colOrder)) {
      .Object@colOrder <- colOrder
    } else {
      .Object@colOrder <- NULL
    }
    if (!missing(colDist)) {
      .Object@colDist <- colDist
    } else {
      .Object@colDist <- "correlation"
    }
    if (!missing(colAgglom)) {
      .Object@colAgglom <- colAgglom
    } else {
      .Object@colAgglom <- "ward.D2"
    }
    if (!missing(rowOrderMethod)) {
      .Object@rowOrderMethod <- rowOrderMethod
    } else {
      .Object@rowOrderMethod <- "User"
    }
    if (!missing(colOrderMethod)) {
      .Object@colOrderMethod <- colOrderMethod
    } else {
      .Object@colOrderMethod <- "User"
    }
    if (!missing(rowCutLocations) & !is.null(rowCutLocations)) {
      if (is(rowCutLocations, "treeCuts")) {
        .Object@rowTreeCuts <- rowCutLocations@numberOfCuts
        .Object@rowCutLocations <- NULL
      } else {
        verifyNumeric(rowCutLocations)
        .Object@rowCutLocations <- castListAsInteger(rowCutLocations)
        .Object@rowTreeCuts <- NULL
      }
    } else {
      .Object@rowCutLocations <- NULL
      .Object@rowTreeCuts <- NULL
    }
    if (!missing(rowCutWidth) & !is.null(rowCutWidth)) {
      verifyNumeric(rowCutWidth)
      .Object@rowCutWidth <- castAsInteger(rowCutWidth)
    } else {
      .Object@rowCutWidth <- 5
    }
    if (!missing(rowTopItems)) {
      .Object@rowTopItems <- rowTopItems
    } else {
      .Object@rowTopItems <- NULL
    }
    if (!missing(rowDisplayLength)) {
      .Object@rowDisplayLength <- rowDisplayLength
    } else {
      .Object@rowDisplayLength <- NULL
    }
    if (!missing(rowDisplayAbbreviation)) {
      .Object@rowDisplayAbbreviation <- rowDisplayAbbreviation
    } else {
      .Object@rowDisplayAbbreviation <- NULL
    }
    if (!missing(colCutLocations) & !is.null(colCutLocations)) {
      if (is(colCutLocations, "treeCuts")) {
        .Object@colTreeCuts <- colCutLocations@numberOfCuts
        .Object@colCutLocations <- NULL
      } else {
        verifyNumeric(colCutLocations)
        .Object@colCutLocations <- castListAsInteger(colCutLocations)
        .Object@colTreeCuts <- NULL
      }
    } else {
      .Object@colCutLocations <- NULL
      .Object@colTreeCuts <- NULL
    }
    if (!missing(colCutWidth) & !is.null(colCutWidth)) {
      verifyNumeric(colCutWidth)
      .Object@colCutWidth <- castAsInteger(colCutWidth)
    } else {
      .Object@colCutWidth <- 5
    }
    if (!missing(colTopItems)) {
      .Object@colTopItems <- colTopItems
    } else {
      .Object@colTopItems <- NULL
    }
    if (!missing(colDisplayLength)) {
      .Object@colDisplayLength <- colDisplayLength
    } else {
      .Object@colDisplayLength <- NULL
    }
    if (!missing(colDisplayAbbreviation)) {
      .Object@colDisplayAbbreviation <- colDisplayAbbreviation
    } else {
      .Object@colDisplayAbbreviation <- NULL
    }
    if (!missing(rowMeta)) {
      .Object@rowMeta <- rowMeta
    } else {
      .Object@rowMeta <- NULL
    }
    if (!missing(colMeta)) {
      .Object@colMeta <- colMeta
    } else {
      .Object@colMeta <- NULL
    }
    if (!missing(axisTypes)) {
      .Object@axisType <- axisTypes
    } else {
      .Object@axisTypes <- NULL
    }
    if (!missing(datasets)) {
      .Object@datasets <- datasets
    } else {
      .Object@datasets <- NULL
    }
    if (!missing(dialogs)) {
      .Object@dialogs <- dialogs
    } else {
      .Object@dialogs <- NULL
    }
    if (!missing(tags)) {
      .Object@tags <- tags
    } else {
      .Object@tags <- c()
    }
    if (!missing(css)) {
      .Object@css <- css
    } else {
      .Object@css <- c()
    }
    if (!missing(rowTypeFunctions)) {
      .Object@rowTypeFunctions <- rowTypeFunctions
    } else {
      .Object@rowTypeFunctions <- NULL
    }
    if (!missing(colTypeFunctions)) {
      .Object@colTypeFunctions <- colTypeFunctions
    } else {
      .Object@colTypeFunctions <- NULL
    }
    if (!missing(elementTypeFunctions)) {
      .Object@elementTypeFunctions <- elementTypeFunctions
    } else {
      .Object@elementTypeFunctions <- NULL
    }
    if (!missing(extrafiles)) {
      .Object@extrafiles <- extrafiles
    } else {
      .Object@extrafiles <- c()
    }
    if (!missing(extrascripts)) {
      .Object@extrascripts <- extrascripts
    } else {
      .Object@extrascripts <- c()
    }
    if (!missing(properties)) {
      .Object@properties <- properties
    } else {
      .Object@properties <- c()
    }
    if (!missing(overviews)) {
      .Object@overviews <- overviews
    } else {
      .Object@overviews <- NULL
    }
    if (!missing(relatedLinks)) {
      .Object@relatedLinks <- relatedLinks
    } else {
      .Object@relatedLinks <- NULL
    }
    if (!missing(relatedGroups)) {
      .Object@relatedGroups <- relatedGroups
    } else {
      .Object@relatedGroups <- NULL
    }
    if (!missing(templates)) {
      .Object@templates <- templates
    } else {
      .Object@templates <- NULL
    }
    if (!missing(width)) {
      verifyNumeric(width)
      .Object@width <- castAsInteger(width)
    } else {
      .Object@width <- as.integer(500)
    }
    if (!missing(height)) {
      verifyNumeric(height)
      .Object@height <- castAsInteger(height)
    } else {
      .Object@height <- as.integer(500)
    }
    if (!missing(panel_configuration)) {
      .Object@panel_configuration <- panel_configuration
    } else {
      .Object@panel_configuration <- default_panel_configuration()
    }
    return(.Object)
  }
)
setIs("ngchmVersion2", "ngchm")
#' Helper class for setting row/col gap locations as tree cuts
#'
#' This class is to facilitate specification of row/col gaps in [chmNew()].
#' Note: user-facing function use the term 'gap', while internal functions that
#' interact with java programs in the NGCHM viewer project use the term 'cut'.
#'
#' @name treeCuts-class
#' @rdname treeCuts-class
#' @slot numberOfCuts Integer number of cuts
#' @seealso [chmNew()]
#' @seealso [chmTreeGaps()]
setClass(Class = "treeCuts", slots = list(numberOfCuts = "optInteger"))
setMethod(
  "initialize", "treeCuts",
  function(.Object, numberOfCuts) {
    .Object@numberOfCuts <- as.integer(numberOfCuts)
    return(.Object)
  }
)

setMethod("show",
  signature = c("ngchm"),
  definition = function(object) {
    cat(sprintf("ngchm %s (%d layers)\n", object@name, length(object@layers)))
  }
)
axisSlots <- c(
  "OrderMethod", "Order", "Dist", "Agglom", "Meta", "CovariateBars", "Dendrogram",
  "CutLocations", "CutWidth", "TreeCuts", "TopItems", "DisplayLength", "DisplayAbbreviation"
)
axisNames <- c(
  "order_method", "labels", "distance_metric", "agglomeration_method", "meta", "covariates", "dendrogram",
  "cut_locations", "cut_width", "tree_cuts", "top_items", "display_length", "display_abbreviation"
)
getAxisData <- function(l, axis) {
  f <- function(name) {
    l[[paste0(axis, name)]]
  }
  axisData <- lapply(axisSlots, f)
  names(axisData) <- axisNames
  axisData$labels <- listFix(s4ToList(axisData$labels), single = c("class", "type", "value"))

  numericElements <- c("cut_locations", "cut_width", "tree_cuts", "display_length")
  for (elem in numericElements) {
    if (!identical(axisData[[elem]], NULL)) {
      axisData[[elem]] <- paste(axisData[[elem]], collapse = ",")
    }
  }

  singleElements <- c(
    "order_method", "distance_metric", "agglomeration_method", "cut_locations", "cut_width",
    "tree_cuts", "display_length", "display_abbreviation"
  )
  for (elem in singleElements) {
    if (!identical(axisData[[elem]], NULL)) {
      class(axisData[[elem]]) <- "singleElement"
    }
  }

  labelElements <- c("display_length", "display_abbreviation")
  for (elem in labelElements) {
    if (!identical(axisData[[elem]], NULL)) {
      axisData$labels[[elem]] <- axisData[[elem]]
      axisData[[elem]] <- NULL
    }
  }


  empty <- vapply(axisData, function(x) length(x) == 0, TRUE)
  if (any(empty)) axisData <- axisData[-which(empty)]
  axisData
}
setMethod(jsonlite:::asJSON, signature = c("singleElement"), definition = function(x, ...) {
  stopifnot(length(x) == 1)
  toJSON(unbox(x[1]), digits = I(7))
})
setMethod(jsonlite:::asJSON, signature = c("ngchmVersion2"), definition = function(x, ...) {
  l <- s4ToList(x)
  l$class <- "ngchm"
  l <- prepChmOrderings(x, l)
  l$layers <- mapply(function(layer) prepDataLayer(x, layer), x@layers, SIMPLIFY = FALSE)
  l$`row_data` <- getAxisData(l, "row")
  l$`col_data` <- getAxisData(l, "col")
  l$tags <- lapply(l$tags, function(t) {
    f <- strsplit(t, "=")[[1]]
    new("ngchmProperty", label = f[1], value = paste(f[-1], collapse = "="))
  })
  singleElements <- c("class", "name", "version")
  for (elem in singleElements) {
    if (!identical(l[[elem]], NULL)) {
      class(l[[elem]]) <- "singleElement"
    }
  }
  l$`type_mappers` <- ngchm.env$typeMappers
  rr <- NULL
  for (cv in x@rowCovariateBars) rr <- appendRendererIfNew(rr, cv@colors)
  for (cv in x@colCovariateBars) rr <- appendRendererIfNew(rr, cv@colors)
  for (ds in x@datasets) {
    for (cv in ds@row.covariates) rr <- appendRendererIfNew(rr, cv@series.properties)
    for (cv in ds@column.covariates) rr <- appendRendererIfNew(rr, cv@series.properties)
  }
  l$`covariate_renderers` <- rr
  l$renderers <- l$colormaps
  ngchm.env$covariateRenderers <- rr
  slotsToExclude <- c(
    "width", "height", "uuid", "baggage", "inpDir", "outDir", "saveDir", "propFile", "css", "format",
    "javascript", "extrafiles", "colormaps",
    vapply(axisSlots, function(x) paste0("row", x), ""),
    vapply(axisSlots, function(x) paste0("col", x), "")
  )
  exclude <- vapply(names(l), function(x) x %in% slotsToExclude, TRUE)
  empty <- vapply(l, function(x) length(x) == 0, TRUE)
  if (any(empty | exclude)) l <- l[-which(empty | exclude)]
  toJSON(l, pretty = TRUE)
})

#' Class representing a deployment method for a Next Generation Clustered Heat Map (NGCHM) server.
#'
#' @exportClass ngchmServerProtocol
#' @name ngchmServerProtocol-class
#' @rdname ngchmServerProtocol-class
#'
#' @keywords classes
setClass(
  "ngchmServerProtocol",
  representation(
    protocolName = "character",
    chmFormat = "character",
    requiredParams = "optCharacter",
    optionalParams = "optCharacter",
    paramValidator = "function",
    setCredentials = "function",
    findCollection = "function", createCollection = "function",
    installMethod = "function", uninstallMethod = "function",
    makePrivate = "function", makePublic = "function"
  )
)
setMethod("show",
  signature = c("ngchmServerProtocol"),
  definition = function(object) {
    cat(sprintf("ngchmServerProtocol %s\n", object@protocolName))
  }
)
#' Class representing a Next Generation Clustered Heat Map (NGCHM) server.
#'
#' @exportClass ngchmServer
#' @name ngchmServer-class
#' @rdname ngchmServer-class
#'
#' @keywords classes
setClass(
  "ngchmServer",
  representation(
    name = "character",
    serverURL = "optCharacter",
    traceLevel = "optCharacter",
    jarFile = "optCharacter",
    serverProtocol = "ngchmServerProtocol",
    deployServer = "optCharacter",
    viewServer = "optCharacter",
    protoOpts = "optList" # Protocol-specific parameters
  )
)
setMethod("show",
  signature = c("ngchmServer"),
  definition = function(object) {
    cat(sprintf("ngchmServer %s\n", object@name))
  }
)
