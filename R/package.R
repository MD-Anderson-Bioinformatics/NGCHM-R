#' Next Generation Clustered Heat Map (NGCHM) Construction Library
#'
#' NGCHM provides tools for defining the contents of a new NGCHM,
#' and for compiling and installing it on a NGCHM server.  This package
#' is often supplemented by a local package that predefines the
#' available NGCHM servers and provides a library of Javascript functions
#' for adding to label and element popup menus.
#'
#' Typical usage is to create a base NGCHM using chmNew; extend it with at
#' least one ngchmLayer; typically extend it further with an additional
#' ngchmLayer, row and column dendrograms, classification bars, and popup menu entries;
#' compile and install it on an available ngchmServer.
#'
#' @seealso ngchm-class
#' @seealso ngchmServer-class
#'
#' @docType package
#' @name NGCHM
#' @rdname NGCHM-package
#' @aliases NGCHM-package
NULL

#' Sample dataset containing RNA expression data from Affymetrix U133A chips
#' for TCGA Glioblastoma Multiforme samples.
#'
#' @name TCGA-GBM-EXPR
#' @docType data
#' @keywords data
NULL
