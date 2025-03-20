#' Create a default panel configuration
#'
#' Creates a standard two-pane panel configuration with a detail map and a summary map.
#' The panes are arranged horizontally with equal widths (50% each).
#'
#' @return A [panel_configuration] object with: two panes ("pane1" and "pane2"), equal
#'         sizes (50% width each), pane1 configured as a detail map, pane2 configured
#'         as a summary map
#' @examples
#'  matrix_data <- matrix(rnorm(100),
#'    nrow = 10, ncol = 10,
#'    dimnames = list(paste0("r", 1:10), paste0("c", 1:10))
#'  )
#' panel_configuration <- default_panel_configuration()
#' hm <- chmNew("my_ngchm", matrix_data, panel_configuration = panel_configuration)
#'
#' @seealso
#' [panel_configuration] for creating custom panel configurations.
#' [detailMap] for detail map configuration.
#' [summaryMap] for summary map configuration.
#'
#' @export
default_panel_configuration <- function() {
  panel_configuration(
    list(
      pane(id = "pane1"),
      pane(id = "pane2"),
      sizes = c(50, 50)
    ),
    list(
      pane1 = detailMap(id = "pane1"),
      pane2 = summaryMap(id = "pane2")
    )
  )
}

#' Build container hierarchy from nested pane list
#'
#' Recursively builds a container hierarchy from a nested list of panes and sizes.
#' This function converts a list structure into a tree of container objects. This
#' function is designed for use in the asJSON method of the [panel_configuration] class.
#'
#' @details
#' The function alternates the 'vertical' property of containers based on depth:
#' * Depth 1: horizontal (vertical = FALSE)
#' * Depth 2: vertical (vertical = TRUE)
#' * Depth 3: horizontal (vertical = FALSE)
#' And so on...
#'
#' @param list_element A list containing panes and optional 'sizes' elements
#' @param container_ids Vector of existing container IDs (for generating unique IDs)
#' @param depth Current depth in the container hierarchy (starts at 1)
#' @param size Size value (width or height) to apply to the current element
#'
#' @return A container object with the complete hierarchy
#'
#' @examples
#' # Create a simple two-pane layout
#' panes <- list(
#'   pane(id = "pane1"),
#'   pane(id = "pane2"),
#'   sizes = c(50, 50)
#' )
#' container <- build_container_hierarchy(panes)
#'
#' @seealso
#' The asJSON method for the [panel_configuration] class for converting the configuration to JSON.
#'
#' @keywords internal
#' @noRd
build_container_hierarchy <- function(list_element, container_ids = c(), depth = 1, size = NULL) {
  vertical <- ifelse(depth %% 2 == 0, TRUE, FALSE)
  if (is.list(list_element)) {
    sizes <- validate_sizes(list_element)
    list_element <- list_element[!names(list_element) %in% c("sizes")]
    container_id <- paste0("ngChmContainer", length(container_ids) + 1)
    container_ids <- c(container_ids, container_id)
    children <- lapply(seq_along(list_element), function(idx) {
      child <- list_element[[idx]]
      pane_size <- if (!is.null(sizes)) sizes[idx] else NULL
      result <- build_container_hierarchy(child, container_ids, depth + 1, pane_size)
    })
    container_obj <- container(id = container_id, vertical = vertical, children = children)
    if (!is.null(size)) {
      if (!vertical) {
        container_obj@height <- size
      } else {
        container_obj@width <- size
      }
    }
    validate_child_panes(container_obj, depth)
    return(container_obj)
  } else if (is(list_element, "pane")) {
    if (vertical) {
      list_element@width <- size
    } else {
      list_element@height <- size
    }
    return(list_element)
  } else {
    log_error("Invalid class of input element to build_container_hierarchy. Must be a list or a pane.")
    stop("Invalid class of input element in build_container_hierarchy. Must be a list or a pane.")
  }
}

#' Validate sizes of panes in a container
#'
#' This function checks that the sizes are a list of integers and sum to 100.
#' If the sizes are not valid, an error is raised.
#'
#' @param list_element The container object to validate.
#' @return A list of validated sizes.
#' @noRd
#' @keywords internal
validate_sizes <- function(list_element) {
  sizes <- list_element$sizes
  if (is.null(sizes)) {
    return(TRUE)
  } # no sizes specified, list_element was a pane
  sizes <- castListAsInteger(sizes, "Pane sizes must be a list of integers.")
  n_panes <- sum(sapply(list_element, function(x) is(x, "pane") || is(x, "list")))
  if (length(sizes) != n_panes) {
    log_error("sizes must be the same length as the number of panes.")
    stop("sizes must be the same length as the number of panes.")
  }
  if (sum(sizes) != 100) {
    log_error("sizes must sum to 100.")
    stop("sizes must sum to 100.")
  }
  return(sizes)
}

#' Validate child panes of a container
#'
#' This function checks the properties of child panes within a container object.
#' It ensures that only one pane can be expanded at a time and that at least one pane is not collapsed.
#'
#' @param container_obj The container object to validate.
#' @param depth The current depth of the container in the nested structure.
#' @return TRUE if the validation passes, otherwise an error is raised.
#' @noRd
#' @keywords internal
validate_child_panes <- function(container_obj, depth) {
  if (depth != 1) {
    return(TRUE)
  } # only validate from the top level of the container
  all_panes <- get_all_panes(container_obj)
  expanded_slots <- sapply(all_panes, function(pane) pane@expanded)
  if (sum(expanded_slots) > 1) {
    log_error("Only one pane can have expanded = true")
    stop("Only one pane can have expanded = true")
  }
  collapsed_slots <- sapply(all_panes, function(pane) pane@collapsed)
  if (sum(collapsed_slots) == length(all_panes)) {
    log_error("At least one pane must have collapsed = false")
    stop("At least one pane must have collapsed = false")
  }
  return(TRUE)
}

#' Helper function to get all panes in a container object
#'
#' This function recursively retrieves all panes within a container object.
#'
#' @param container_obj The container object to search for panes.
#' @return A list of all panes found within the container object.
#' @noRd
#' @keywords internal
get_all_panes <- function(container_obj) {
  all_children <- list()
  if (!is.null(slot(container_obj, "children"))) {
    for (child in slot(container_obj, "children")) {
      if (is(child, "container")) {
        all_children <- c(all_children, get_all_panes(child))
      } else {
        all_children <- c(all_children, list(child))
      }
    }
  }
  return(all_children)
}

