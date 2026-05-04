# Next Generation Clustered Heat Map (NGCHM) Construction Library

NGCHM provides tools for defining the contents of a new NGCHM, and for
compiling and installing it on a NGCHM server.

## Details

Typical usage (see example) is to create a base NGCHM using chmNew;
extend it with at least one ngchmLayer; typically extend it further with
an additional ngchmLayer, row and column dendrograms, classification
bars, and popup menu entries; compile and install it on an available
ngchmServer.

Note:

- [`chmNew()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNew.md)
  requires **git** to be installed.

- [`chmExportToFile()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmExportToFile-method.md),
  [`chmExportToHTML()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmExportToHTML-method.md),
  and
  [`chmExportToPDF()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmExportToPDF-method.md)
  require **Java 11** and the
  **[NGCHMSupportFiles](https://github.com/MD-Anderson-Bioinformatics/NGCHMSupportFiles)**
  package. The NGCHMSupportFiles package can be installed with:  
    
  `install.packages('NGCHMDemoData', `  
  `repos = c('https://md-anderson-bioinformatics.r-universe.dev',`  
  `'https://cloud.r-project.org'))`

## Initialization

When first loaded the NGCHM library reads configuration files in the
directories specified by the NGCHMCONFIGPATH environment variable. This
is a colon (:) separated list of directory names. If not set it defaults
to /etc/ngchm:/usr/local/ngchm:/opt/ngchm:\$HOME/.ngchm. See
NGCHM-initialization for details.

## See also

[`chmNew()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNew.md)

[`chmAdd()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAdd-method.md)

[`chmExportToFile()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmExportToFile-method.md)

[`chmExportToPDF()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmExportToPDF-method.md)

[`chmSetCollection()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmSetCollection.md)

[`chmInstall()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmInstall-method.md)

[ngchm](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchm-class.md)

## Examples

``` r
# Examples using `chmNew()` require git to be installed.
# The NGCHMSupportFiles package is required by chmExportToFile and chmExportToPDF
# The NGCHMDemoData package is used to create a demo NGCHM
if (FALSE) { # \dontrun{
  if (requireNamespace("NGCHMSupportFiles", quietly = TRUE)) {
    if (requireNamespace("NGCHMDemoData", quietly = TRUE)) {
      library(NGCHMSupportFiles)
      library(NGCHMDemoData)
      data(TCGA.GBM.EXPR, package = "NGCHMDemoData")
      chm1 <- chmNew("gbm", TCGA.GBM.EXPR[1:50, 1:50],
        rowAxisType = "bio.gene.hugo",
        colAxisType = "bio.tcga.barcode.sample.vial.portion.analyte.aliquot"
      )
      chmExportToFile(chm1, tempfile("gbm", fileext = ".ngchm"))
      chmExportToPDF(chm1, tempfile("gbm", fileext = ".pdf"))
    }
  }
  mat <- matrix(rnorm(100), nrow = 10)
  rownames(mat) <- sprintf("ABCA%d", 1:10)
  colnames(mat) <- sprintf("Sample%d", 1:10)
  chm <- chmNew("my-chm", mat)
} # }
```
