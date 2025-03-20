# The mapConfig.json file in a .ngchm zip file has a key `panel_configuration` with keys:
#
#   - `panel_layout`, a top-top-level horizontal container with id = "ngChmContainer"
#                     with one child: top-level horizontal container with id = "ngChmContainer1".
#                     Children of "ngChmContainer1" are containers / panes.
#                     The 'ngChmContainer1' represenst tree-structure of the panel layout.
#   - keys for each of the panes, (e.g. `pane1`, `pane2`, etc.) which contain the actual panes
#   - `flickInfo`, containing the flick state info. This is optional and not implemented here.
#   - `selections`, containing selected items. This is optional and not implemented here.
#
# The classes in this file relate to the `panel_layout` key and the keys for the individual
# panes (`pane1`, `pane2`, etc.)

#' Panel Configuration Class for NG-CHM
#'
#' This configuration appears in the `panel_configuration` key of the mapConfig.json file under keys:
#' * `panel_layout`: The hierarchical structure of containers and panes. The `panes_list` slot is converted to this structure.
#' * Individual pane configurations (e.g., `pane1`, `pane2`) containing pane-specific settings. The `pane_types`
#'   slot is converted to these individual pane configurations.
#'
#' @slot panes_list A list containing pane objects and sizes information.
#' @slot pane_types A named list mapping pane IDs to their configurations.
#'
#' @seealso
#' * [detailMap] for detail map configuration
#' * [summaryMap] for summary map configuration
#' * [pluginPane] for plugin pane configuration
#'
#' @exportClass panel_configuration
setClass("panel_configuration",
  slots = list(
    panes_list = "list",
    pane_types = "list"
  )
)

#' Constructor for panel_configuration
#'
#' Creates a new panel configuration object that combines pane layout information
#' with individual pane specifications.
#'
#' @param panes_list A list containing pane objects and sizes information, where sizes is expressed as a percentage.
#'        For example, this panes_list format: \cr\cr
#'        \preformatted{
#'          list(pane(id="pane1"),
#'               pane(id="pane2"),
#'               sizes = c(40, 60))
#'        }
#'        will produce a layout like the one below, where `pane1` accounts for 40% of the horizontal space
#'        and `pane2` accounts for 60% of the horizontal space:
#'           \preformatted{
#'                       |
#'                 pane1 | pane2
#'                       |
#'           }
#'        This panes_list format:
#'           \preformatted{
#'             list(pane(id="pane1"),
#'                  list(pane(id="pane2"),
#'                       pane(id="pane3"),
#'                       sizes = c(30, 70)), # for pane2, pane3
#'             sizes = c(50, 50))  # for pane1 & container of pane2, pane3
#'           }
#'        will produce a layout like the one below, where `pane2` accounts for 30% of
#'        the vertical space and `pane3` accounts for 70% of the vertical space:
#'         \preformatted{
#'                         |  pane2
#'                  pane1  | -------
#'                         |  pane3
#'        }
#' @param pane_types A named list mapping pane IDs to their configurations.
#'        Each element must be a detailMap, summaryMap, or pluginPane object.
#'        Each element must be a detailMap, summaryMap, or pluginPane object.
#'        For example, the default two-pane layout is a detailMap and a summaryMap:
#'           \preformatted{
#'             pane_types = list(
#'                    pane1 = detailMap(id = "pane1"),
#'                    pane2 = summaryMap(id = "pane2")
#'             )
#'           }
#'        For example, a three-pane layout with a detail map, a summary map, and a plugin pane:
#'           \preformatted{
#'             pane_types = list(
#'                    pane1 = detailMap(id = "pane1"),
#'                    pane2 = summaryMap(id = "pane2"),
#'                    pane3 = pluginPane(id = "pane3",
#'                       pluginName = "2D ScatterPlot: UMAP (column)")
#'             )
#'           }
#'
#' @return A new \code{panel_configuration} object
#'
#' @examples
#' # Create a three-pane layout with a detail map, a summary map, and a plugin pane:
#' pane1 <- pane(id = "pane1")
#' pane2 <- pane(id = "pane2")
#' pane3 <- pane(id = "pane3")
#' panes_list <- list(pane1, list(pane2, pane3, sizes = c(30, 70)), sizes = c(50, 50))
#'
#' # Create pane configuration for each pane
#' pane_types <- list(
#'   pane1 = detailMap(id = "pane1"),
#'   pane2 = summaryMap(id = "pane2"),
#'   pane3 = pluginPane(id = "pane3", pluginName = "2D ScatterPlot: UMAP (column)")
#' )
#'
#' # Create panel configuration
#' config <- panel_configuration(panes_list, pane_types)
#'
#' # Create ngchm with the panel configuration
#' matrix <- matrix(rnorm(100),
#'   nrow = 10, ncol = 10,
#'   dimnames = list(paste0("r", 1:10), paste0("c", 1:10))
#' )
#' hm <- chmNew("three-panel-ngchm", matrix, panel_configuration = config)
#'
#' @seealso
#' * [panel_configuration-class] for class details
#' * [pane] for creating pane objects
#' * [detailMap] for detail map configuration
#' * [summaryMap] for summary map configuration
#' * [pluginPane] for plugin pane configuration
#'
#' @export
panel_configuration <- function(panes_list, pane_types) {
  pane_ids <- as.character(sort(get_all_pane_ids(panes_list)))
  types_ids <- as.character(sort(names(pane_types)))
  if (!identical(pane_ids, types_ids)) {
    stop("The ids from panes_list and pane_types must match.")
  }
  new("panel_configuration", panes_list = panes_list, pane_types = pane_types)
}

#' Container Class for Panel Layout
#'
#' The container class is part of the panel layout system in NG-CHM. It is used to organize
#' panes in a hierarchical structure. Each container can be either vertical or horizontal,
#' and can contain multiple children (panes or other containers).
#'
#' @slot children A list of child objects (panes or other containers)
#' @slot height Numeric value for container height (as percentage, 0-100)
#' @slot id Character string identifying the container (e.g. "ngChmContainer1")
#' @slot vertical Logical indicating container is vertical (TRUE) or horizontal (FALSE)
#' @slot width Numeric value for container width (as percentage, 0-100)
#'
#' @seealso
#' * [pane] for leaf nodes in the layout tree
setClass("container",
  slots = list(
    children = "list",
    height = "numeric",
    id = "character",
    vertical = "logical",
    width = "numeric"
  )
)

#' Create a container object for panel layout
#'
#' Creates a new container object that can hold panes or other containers in the NG-CHM panel layout system.
#' A container organizes its children either vertically or horizontally and specifies their dimensions
#' as percentages.
#'
#' @param children A list of child objects (panes or other containers). Default is an empty list.
#' @param height Numeric value for container height, expressed as a percentage (0-100). Default is 100.
#' @param id Character string identifying the container. Must be specified, no default.
#' @param vertical Logical indicating if children are arranged vertically (TRUE) or horizontally (FALSE). Default is FALSE.
#' @param width Numeric value for container width, expressed as a percentage (0-100). Default is 100.
#'
#' @return A new `container` object with the specified properties.
#' @keywords internal
container <- function(children = list(), height = 100, id = NA, vertical = FALSE, width = 100) {
  if (is.na(id)) {
    log_error("'id' must be specified when creating a 'container' object.")
    stop("'id' must be specified when creating a 'container' object.")
  }
  width <- as.numeric(width)
  if (width < 0 || width > 100) {
    log_error("'width' is expressed as a percentage, and must be between 0 and 100.")
    stop("'width' is expressed as a percentage, and must be between 0 and 100.")
  }
  height <- as.numeric(height)
  if (height < 0 || height > 100) {
    log_error("'height' is expressed as a percentage, and must be between 0 and 100.")
    stop("'height' is expressed as a percentage, and must be between 0 and 100.")
  }
  new("container", id = id, width = width, height = height, vertical = vertical, children = children)
}

# Convert container object to JSON
setMethod(jsonlite:::asJSON, "container", function(x, ...) {
  list_representation <- list(
    id = slot(x, "id"),
    width = paste0(slot(x, "width"), "%"),
    height = paste0(slot(x, "height"), "%"),
    type = "container",
    vertical = slot(x, "vertical"),
    children = lapply(slot(x, "children"), function(child) {
      if (is(child, "container") || is(child, "pane")) {
        jsonlite::fromJSON(jsonlite::toJSON(child, ...))
      } else {
        child
      }
    })
  )
  jsonlite::toJSON(list_representation, auto_unbox = TRUE, pretty = TRUE)
})

#' Panel Layout Class for NG-CHM
#'
#' A specialized container class that represents the top-top-level layout structure in the NG-CHM
#' panel configuration. Inherits from the container class, but provides a custom JSON
#' serialization that enforces the required top-top-level container structure.
#'
#' The panel_layout class creates the required two-level container hierarchy:
#' - A top-top-level container with id "ngChmContainer"
#' - A child container that holds the actual panel layout structure
#'
#' This class inherits all slots from the container class but provides a specialized
#' JSON serialization method to ensure proper formatting for the NG-CHM viewer.
#'
#' @seealso
#' * [container] for the parent class
#' * [panel_configuration] for the complete panel configuration
#'
#' @keywords internal
setClass("panel_layout", contains = "container")

# Convert panel_layout object to JSON
setMethod(jsonlite:::asJSON, "panel_layout", function(x, ...) {
  top_level_list <- list(
    id = "ngChmContainer",
    width = "100%",
    height = "100%",
    type = "container",
    vertical = FALSE,
    children = lapply(slot(x, "children"), function(child) {
      if (is(child, "container") || is(child, "pane")) {
        jsonlite::fromJSON(jsonlite::toJSON(child, ...))
      } else {
        child
      }
    })
  )
  jsonlite::toJSON(top_level_list, auto_unbox = TRUE, pretty = TRUE)
})

#' Helper function to get all pane IDs from a panes_list
#' @noRd
#' @keywords internal
get_all_pane_ids <- function(panes_list) {
  ids <- Filter(
    function(x) !is.null(x),
    lapply(unlist(panes_list), function(x) {
      if (is(x, "pane")) {
        slot(x, "id")
      } else {
        NULL
      }
    })
  )
  unlist(ids)
}

# Convert panel_configuration object to JSON
setMethod(jsonlite:::asJSON, "panel_configuration", function(x, ...) {
  # this is where that 'top-top-level' container with id = "ngChmContainer" is created.
  ngChmContainer1 <- build_container_hierarchy(slot(x, "panes_list"))
  layout <- new("panel_layout",
    id = "ngChmContainer",
    children = c(ngChmContainer1),
    vertical = FALSE
  )
  panel_configuration_value <- list(
    panel_layout = jsonlite::fromJSON(jsonlite::toJSON(layout, ...))
  )
  for (pane_id in names(slot(x, "pane_types"))) {
    panel_configuration_value[[pane_id]] <- jsonlite::fromJSON(
      jsonlite::toJSON(slot(x, "pane_types")[[pane_id]], ...)
    )
  }
  jsonlite::toJSON(panel_configuration_value, auto_unbox = TRUE, pretty = TRUE)
})

#' Pane Class for NG-CHM Layout
#'
#' A pane represents a single viewing area in the NG-CHM panel layout system.
#'
#' Panes are the leaf nodes in the panel layout tree structure. They are contained
#' within containers and can be configured as detail maps, summary maps, or plugin panes.
#'
#' @slot collapsed Logical indicating if the pane is collapsed
#' @slot expanded Logical indicating if the pane is expanded
#' @slot height Numeric value for pane height (as percentage, 0-100)
#' @slot id Character string identifying the pane
#' @slot width Numeric value for pane width (as percentage, 0-100)
#'
#' @seealso
#' * [container] for parent container class
#' * [panel_configuration] for complete configuration
#'
#' @examples
#' # Create a basic pane
#' p1 <- pane(id = "pane1", width = 50, height = 100)
#'
#' @exportClass pane
setClass("pane",
  slots = list(
    collapsed = "logical",
    expanded = "logical",
    height = "numeric",
    id = "character",
    width = "numeric"
  )
)

#' Create a pane for NG-CHM layout
#'
#' Creates a new pane object representing a single viewing area in the NG-CHM panel layout system.
#'
#' @param id Character string identifying the pane. Must be specified, no default.
#' @param width Numeric value for pane width, expressed as a percentage (0-100). Default is 100.
#' @param height Numeric value for pane height, expressed as a percentage (0-100). Default is 100.
#' @param collapsed Logical indicating if the pane is collapsed. Default is FALSE.
#' @param expanded Logical indicating if the pane is expanded. Default is FALSE.
#'
#' @return A new `pane` object with the specified properties
#'
#' @examples
#' # Create a basic pane
#' p1 <- pane(id = "pane1")
#'
#' # Create a half-width pane
#' p2 <- pane(id = "pane2", width = 50)
#'
#' # Create an expanded pane
#' p3 <- pane(id = "pane3", expanded = TRUE)
#'
#' @seealso
#' * [pane-class] for class details
#' * [panel_configuration] for using panes in configurations
#'
#' @export
pane <- function(id = NA, width = 100, height = 100, collapsed = FALSE, expanded = FALSE) {
  if (is.na(id)) {
    log_error("'id' must be specified when creating a 'pane' object.")
    stop("'id' must be specified when creating a 'pane' object.")
  }
  width <- as.numeric(width)
  if (width < 0 || width > 100) {
    log_error("'width' is expressed as a percentage, and must be between 0 and 100.")
    stop("'width' is expressed as a percentage, and must be between 0 and 100.")
  }
  height <- as.numeric(height)
  if (height < 0 || height > 100) {
    log_error("'height' is expressed as a percentage, and must be between 0 and 100.")
    stop("'height' is expressed as a percentage, and must be between 0 and 100.")
  }
  new("pane", id = id, width = width, height = height, collapsed = collapsed, expanded = expanded)
}

# Convert pane object to JSON
setMethod(jsonlite:::asJSON, "pane", function(x, ...) {
  list_representation <- list(
    id = slot(x, "id"),
    width = paste0(slot(x, "width"), "%"),
    height = paste0(slot(x, "height"), "%"),
    type = "pane",
    collapsed = slot(x, "collapsed"),
    expanded = slot(x, "expanded")
  )
  invisible(jsonlite::toJSON(list_representation))
})

#' Plugin Pane Class for NG-CHM Layout
#'
#' Represents a plugin pane in the NG-CHM panel layout system. Plugin panes allow
#' integration of additional visualization components like scatter plots, bar charts,
#' or other custom visualizations.
#'
#' @slot id Character string identifying the plugin pane
#' @slot pluginName Character string specifying the plugin type (e.g., "2D ScatterPlot: UMAP (column)")
#'
#' @seealso
#' * [panel_configuration] for using plugin panes in configurations
#' * [detailMap] for detail map configuration
#' * [summaryMap] for summary map configuration
#' 
#' @exportClass pluginPane
setClass("pluginPane",
  slots = list(
    id = "character",
    pluginName = "character"
  )
)

#' Create a plugin pane for NG-CHM layout
#'
#' Creates a new plugin pane object that represents a specialized visualization
#' component in the NG-CHM panel layout system, such as scatter plots or bar charts.
#'
#' @param id Character string identifying the plugin pane. Must be specified, no default.
#' @param pluginName Character string specifying the plugin type (e.g., "2D ScatterPlot: UMAP (column)").
#'        Must be specified, no default.
#'
#' @return A new `pluginPane` object with the specified properties
#'
#' @examples
#' # Create a UMAP scatter plot plugin pane
#' plugin <- pluginPane(
#'   id = "pane3",
#'   pluginName = "2D ScatterPlot: UMAP (column)"
#' )
#'
#' @seealso
#' * [pluginPane-class] for class details
#' * [panel_configuration] for using plugin panes in configurations
#'
#' @export
pluginPane <- function(id = NA, pluginName = NA) {
  if (is.na(id)) {
    log_error("'id' must be specified when creating a 'pluginPane' object.")
    stop("'id' must be specified when creating a 'pluginPane' object.")
  }
  if (is.na(pluginName)) {
    log_error("'pluginName' must be specified when creating a 'pluginPane' object.")
    stop("'pluginName' must be specified when creating a 'pluginPane' object.")
  }
  new("pluginPane", id = id, pluginName = pluginName)
}

#' summaryMap class
#'
#' This is a helper class for creating the 'panel_configuration' part of the mapConfig.json file.
#' This class represents a 'summaryMap' entry.
#'
#' @slot id The ID of the summary map. This must be specified.
#' @name summaryMap-class
#' @exportClass summaryMap
setClass("summaryMap",
  slots = list(
    id = "character"
  )
)

#' Create a summary map pane
#'
#' Creates a new summary map object for use in NG-CHM panel configurations.
#'
#' @param id Character string identifying the summary map pane. Must be specified, no default.
#'
#' @return A new `summaryMap` object with the specified ID
#'
#' @examples
#' # Create a summary map pane
#' summary <- summaryMap(id = "pane2")
#'
#' @seealso
#' * [summaryMap-class] for class details
#' * [panel_configuration] for using summary maps in configurations
#' * [detailMap] for detail map panes
#'
#' @export
summaryMap <- function(id = NA) {
  if (is.na(id)) {
    log_error("'id' must be specified when creating a 'summaryMap' object.")
    stop("'id' must be specified when creating a 'summaryMap' object.")
  }
  new("summaryMap", id = id)
}

#' Detail Map Class for NG-CHM Layout
#'
#' Represents a detail map pane in the NG-CHM panel layout system. Detail maps
#' provide a zoomed-in view of a portion of the heatmap with configurable display settings.
#'
#' @slot currentCol Integer. Current column position in the heatmap (must be > 0)
#' @slot currentRow Integer. Current row position in the heatmap (must be > 0)
#' @slot colZoomLevel Numeric. Column zoom level between 0 and 1
#' @slot dataBoxHeight Integer. Height of each data box. Must be one of the predefined sizes from NG-CHM Viewer
#' @slot dataBoxWidth Integer. Width of each data box. Must be one of the predefined sizes from NG-CHM Viewer
#' @slot dataPerCol Integer. Number of rows displayed in detail map (must be > 1)
#' @slot dataPerRow Integer. Number of columns displayed in detail map (must be > 1)
#' @slot id Character. Unique identifier for the pane
#' @slot mode Character. View mode: "NORMAL", "RIBBONH", or "RIBBONV"
#' @slot rowZoomLevel Numeric. Row zoom level between 0 and 1
#' @slot selectedIsDendrogram Logical. Whether selection is in dendrogram
#' @slot selectedStart Integer. Start position of selection (>= 0)
#' @slot selectedStop Integer. End position of selection (>= 0)
#' @slot version Character. Map version: "P" for primary, "S" for secondary
#' @slot versionNumber Integer. Detail map number for multiple detail panes
#'
#' @seealso
#' * [panel_configuration] for using detail maps in configurations
#' * [summaryMap] for summary map configuration
#'
#' @exportClass detailMap
setClass("detailMap",
  slots = list(
    currentCol = "integer",
    currentRow = "integer",
    colZoomLevel = "numeric",
    dataBoxHeight = "integer", # must be one of the DET.zoomBoxSizes in NG-CHM Viewer code
    dataBoxWidth = "integer", # must be one of the DET.zoomBoxSizes in NG-CHM Viewer code
    dataPerCol = "integer", # number of rows displayed in detail map
    dataPerRow = "integer", # number of columns displayed in detail map
    id = "character",
    mode = "character", # 'NORMAL', 'RIBBONH', 'RIBBONV'
    rowZoomLevel = "numeric",
    selectedIsDendrogram = "logical",
    selectedStart = "integer",
    selectedStop = "integer",
    version = "character", # 'P' for primary, 'S' for others
    versionNumber = "integer" # The detail map number (NGCHM Viewer supports multiple detail panes)
  )
)

#' Create a detail map pane
#'
#' @description
#' Creates a new detail map object that provides a zoomed-in view of a portion of the heatmap
#' with configurable display settings.
#'
#' @param id Character string identifying the detail map pane. Must be specified, no default.
#' @param currentCol Integer. Current column position in heatmap (must be > 0). Default is 1.
#' @param currentRow Integer. Current row position in heatmap (must be > 0). Default is 1.
#' @param colZoomLevel Numeric. Column zoom level between 0 and 1. Default is 1.
#' @param dataBoxHeight Integer. Height of each data box. Must be one of the predefined sizes. Default is 42.
#' @param dataBoxWidth Integer. Width of each data box. Must be one of the predefined sizes. Default is 42.
#' @param dataPerCol Integer. Number of rows displayed in detail map (must be > 1). Default is 12.
#' @param dataPerRow Integer. Number of columns displayed in detail map (must be > 1). Default is 12.
#' @param mode Character. View mode: "NORMAL", "RIBBONH", or "RIBBONV". Default is "NORMAL".
#' @param rowZoomLevel Numeric. Row zoom level between 0 and 1. Default is 1.
#' @param selectedIsDendrogram Logical. Whether selection is in dendrogram. Default is FALSE.
#' @param selectedStart Integer. Start position of selection (>= 0). Default is 0.
#' @param selectedStop Integer. End position of selection (>= 0). Default is 0.
#' @param version Character. Map version: "P" for primary, "S" for secondary. Default is "P".
#' @param versionNumber Integer. Detail map number for multiple detail panes. Default is 1.
#'
#' @return A new `detailMap` object with the specified properties
#'
#' @examples
#' # Create a basic detail map
#' detail <- detailMap(id = "pane1")
#'
#' # Create a detail map with custom zoom and box size
#' detail2 <- detailMap(
#'   id = "pane2",
#'   colZoomLevel = 0.5,
#'   rowZoomLevel = 0.5,
#'   dataBoxHeight = 84,
#'   dataBoxWidth = 84
#' )
#'
#' @seealso
#' * [detailMap-class] for class details
#' * [panel_configuration] for using detail maps in configurations
#'
#' @export
detailMap <- function(id = NA, currentCol = 1, currentRow = 1, colZoomLevel = 1, dataBoxHeight = 42, dataBoxWidth = 42,
                      dataPerCol = 12, dataPerRow = 12,
                      mode = "NORMAL", rowZoomLevel = 1, selectedIsDendrogram = FALSE, selectedStart = 0,
                      selectedStop = 0, version = "P", versionNumber = 1) {
  integer_params <- c(
    "currentCol", "currentRow", "dataBoxHeight", "dataBoxWidth",
    "dataPerCol", "dataPerRow", "selectedStart", "selectedStop", "versionNumber"
  )
  for (integer_param in integer_params) {
    original_value <- get(integer_param)
    integer_value <- as.integer(original_value)
    if (original_value != integer_value) {
      warning(paste0("'", integer_param, "' was not an integer. It has been converted to ", integer_value, "."))
    }
    assign(integer_param, integer_value)
  }
  if (currentCol < 1) {
    log_error("'currentCol' must be an integer > 1.")
    stop("'currentCol' must be an integer > 1.")
  }
  if (currentRow < 1) {
    log_error("'currentRow' must be an integer > 1.")
    stop("'currentRow' must be an integer > 1.")
  }
  if (colZoomLevel < 0 || colZoomLevel > 1) {
    log_error("'colZoomLevel' must be between 0 and 1.")
    stop("'colZoomLevel' must be between 0 and 1.")
  }
  # Box sizes must be same as DET.zoomBoxSizes in NG-CHM Viewer code, sigh.
  # https://github.com/MD-Anderson-Bioinformatics/NG-CHM/blob/main/NGCHM/WebContent/javascript/DetailHeatMapDisplay.js#L26
  boxSizes <- c(1, 2, 3, 4, 6, 7, 8, 9, 12, 14, 18, 21, 24, 28, 36, 42, 56, 63, 72, 84, 126, 168, 252)
  if (!(dataBoxHeight %in% boxSizes)) {
    errorMsg <- paste0("'dataBoxHeight' must be one of ", paste(boxSizes, collapse = ", "), ".")
    log_error(errorMsg)
    stop(errorMsg)
  }
  if (!(dataBoxWidth %in% boxSizes)) {
    errorMsg <- paste0("'dataBoxWidth' must be one of ", paste(boxSizes, collapse = ", "), ".")
    log_error(errorMsg)
    stop(errorMsg)
  }
  if (dataPerCol < 2) {
    log_error("'dataPerCol' must be an integer > 1.")
    stop("'dataPerCol' must be an integer > 1.")
  }
  if (dataPerRow < 2) {
    log_error("'dataPerRow' must be an integer > 1.")
    stop("'dataPerRow' must be an integer > 1.")
  }
  if (is.na(id)) {
    log_error("'id' must be specified when creating a 'detailMap' object.")
    stop("'id' must be specified when creating a 'detailMap' object.")
  }
  if (!mode %in% c("NORMAL", "RIBBONH", "RIBBONV")) {
    log_error("'mode' must be one of 'NORMAL', 'RIBBONH', or 'RIBBONV'.")
    stop("'mode' must be one of 'NORMAL', 'RIBBONH', or 'RIBBONV'.")
  }
  if (rowZoomLevel < 0 || rowZoomLevel > 1) {
    log_error("'rowZoomLevel' must be between 0 and 1.")
    stop("'rowZoomLevel' must be between 0 and 1.")
  }
  if (selectedStart < 0) {
    log_error("'selectedStart' must be a non-negative integer.")
    stop("'selectedStart' must be a non-negative integer.")
  }
  if (selectedStop < 0) {
    log_error("'selectedStop' must be a non-negative integer.")
    stop("'selectedStop' must be a non-negative integer.")
  }
  if (!version %in% c("P", "S")) {
    log_error("'mode' must be one of 'P' or 'S'.")
    stop("'mode' must be one of 'P' or 'S'.")
  }
  new("detailMap",
    currentCol = currentCol, currentRow = currentRow, colZoomLevel = colZoomLevel,
    dataBoxHeight = dataBoxHeight, dataBoxWidth = dataBoxWidth,
    dataPerCol = dataPerCol, dataPerRow = dataPerRow, id = id, mode = mode, rowZoomLevel = rowZoomLevel,
    selectedIsDendrogram = selectedIsDendrogram, selectedStart = selectedStart, selectedStop = selectedStop,
    version = version, versionNumber = versionNumber
  )
}

setClassUnion("panel", c("detailMap", "summaryMap", "pluginPane"))

# Convert detailMap, summaryMap, and pluginPane objects to JSON
setMethod(jsonlite:::asJSON, signature = c("panel"), definition = function(x, ...) {
  if (is(x, "detailMap")) {
    objList <- list(
      currentCol = slot(x, "currentCol"),
      currentRow = slot(x, "currentRow"),
      colZoomLevel = slot(x, "colZoomLevel"),
      dataBoxHeight = slot(x, "dataBoxHeight"),
      dataBoxWidth = slot(x, "dataBoxWidth"),
      dataPerCol = slot(x, "dataPerCol"),
      dataPerRow = slot(x, "dataPerRow"),
      dataViewHeight = 504, # 504 is the value of DET.SIZE_NORMAL_MODE in the NG-CHM Viewer Code
      dataViewWidth = 504, # 504 is the value of DET.SIZE_NORMAL_MODE in the NG-CHM Viewer Code
      mode = slot(x, "mode"),
      rowZoomLevel = slot(x, "rowZoomLevel"),
      selectedIsDendrogram = slot(x, "selectedIsDendrogram"),
      selectedStart = slot(x, "selectedStart"),
      selectedStop = slot(x, "selectedStop"),
      version = slot(x, "version"),
      versionNumber = slot(x, "versionNumber"),
      type = "detailMap"
    )
  } else if (is(x, "summaryMap")) {
    objList <- list(
      type = "summaryMap"
    )
  } else if (is(x, "pluginPane")) {
    objList <- list(
      pluginName = slot(x, "pluginName"),
      type = "plugin" # type required by NGCHM viewer code
    )
  } else {
    stop("Unknown panel type")
  }
  jsonlite::toJSON(objList, auto_unbox = TRUE, pretty = TRUE)
})
