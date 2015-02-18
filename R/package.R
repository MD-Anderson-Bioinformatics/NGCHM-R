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
#' @seealso chmNew
#' @seealso chmAdd
#' @seealso chmMake
#' @seealso chmInstall
#' @seealso chmListServers
#' @seealso ngchm-class
#'
#' @docType package
#' @name NGCHM
#' @rdname NGCHM-package
#' @aliases NGCHM-package
#'
#' @examples
#' mat <- matrix(rnorm(100),nrow=10)
#' rownames(mat) <- sprintf ("ABCA%d", 1:10)
#' colnames(mat) <- sprintf ("Sample%d", 1:10)
#' chm <- chmNew ('my-chm', mat, rowAxisType='bio.gene.hugo')
#'\dontrun{
#' chmMake ('my-server', chm)
#' chmInstall ('my-server', chm)
#'}
NULL

#' Sample dataset containing RNA expression data from Affymetrix U133A chips
#' for TCGA Glioblastoma Multiforme samples.
#'
#' @name TCGA.GBM.EXPR
#' @docType data
#' @keywords data
#'
#' @examples
#' data(TCGA.GBM.EXPR)
#' sample.type <- 'bio.tcga.barcode.sample.vial.portion.analyte.aliquot'
#' chm <- chmNew ('my-gbm-chm', TCGA.GBM.EXPR, rowAxisType='bio.gene.hugo',
#'                 colAxisType=sample.type)
#'\dontrun{
#' chmMake ('my-server', chm)
#' chmInstall ('my-server', chm)
#'}
NULL
