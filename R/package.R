#' Next Generation Clustered Heat Map (NGCHM) Construction Library
#'
#' NGCHM provides tools for defining the contents of a new NGCHM,
#' and for compiling and installing it on a NGCHM server.
#'
#' Typical usage (see example) is to create a base NGCHM using chmNew; extend it with at
#' least one ngchmLayer; typically extend it further with an additional
#' ngchmLayer, row and column dendrograms, classification bars, and popup menu entries;
#' compile and install it on an available ngchmServer.
#'
#' @section Initialization:
#' When first loaded the NGCHM library reads configuration files in
#' the directories specified by the NGCHMCONFIGPATH environment variable.  This is
#' a colon (:) separated list of directory names.  If not set it defaults to
#' /etc/ngchm:/usr/local/ngchm:/opt/ngchm:$HOME/.ngchm.
#' See NGCHM-initialization for details.
#'
#' @seealso [chmNew()]
#' @seealso [chmAdd()]
#' @seealso [chmExportToFile()]
#' @seealso [chmExportToPDF()]
#' @seealso [chmSetCollection()]
#' @seealso [chmInstall()]
#' @seealso [ngchm-class]
#'
#' @docType package
#' @name NGCHM
#' @rdname NGCHM-package
#' @aliases NGCHM-package
#'
#' @import htmltools
#'
#' @examples
#' # The NGCHMSupportFiles package is required by chmExportToFile and chmExportToPDF
#' # The NGCHMDemoData package is used to create a demo NGCHM
#' \dontrun{
#'   library(NGCHMSupportFiles)
#'   data(TCGA.GBM.EXPR, package = 'NGCHMDemoData')
#'   chm1 <- chmNew('gbm', TCGA.GBM.EXPR[1:50,1:50],
#'     rowAxisType = 'bio.gene.hugo',
#'     colAxisType = 'bio.tcga.barcode.sample.vial.portion.analyte.aliquot')
#'   chmExportToFile(chm1, tempfile('gbm', fileext = '.ngchm'))
#'   chmExportToPDF(chm1, tempfile('gbm', fileext = '.pdf'))
#' }
#' mat <- matrix(rnorm(100), nrow = 10)
#' rownames(mat) <- sprintf("ABCA%d", 1:10)
#' colnames(mat) <- sprintf("Sample%d", 1:10)
#' chm <- chmNew('my-chm', mat)
#' \dontrun{chmSetCollection ('//server/collection')
#'   chmInstall (chm)
#' }
NULL
