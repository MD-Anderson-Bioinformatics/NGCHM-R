# Add PCA coordinates to an NG-CHM.

Add PCA coordinates as hidden covariate bars to an axis of an NG-CHM.
One hidden covariate bar is added for each PCA coordinate (up to ndim
coordinates). Coordinates are given names 'BASENAME.coordinate.N', where
BASENAME is specified by the parameter basename (default "PC") and N
ranges from 1 to the number of added covariate bars.

## Usage

``` r
chmAddPCA(hm, axis, prc, basename = "PC", ndim = 2)
```

## Arguments

- hm:

  The NGCHM to add the coordinates to.

- axis:

  The NGCHM axis ("row" or "column") to add the coordinates to.

- prc:

  Principal component coordinates (output of
  [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html)) for the
  specified NGCHM axis.

- basename:

  The prefix to use for the coordinate names.

- ndim:

  The maximum number of coordinates to add.

## Value

The NGCHM with added coordinates.

## See also

[`chmAddTSNE()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddTSNE.md)

[`chmAddUMAP()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddUMAP.md)

[`chmAddUWOT()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddUWOT.md)

[`chmAddReducedDim()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddReducedDim.md)

## Examples

``` r
# Examples using `chmNew()` require git to be installed.
if (FALSE) { # \dontrun{
  # If the NGCHMDemoData package is installed, use it to demo usage
  if (requireNamespace("NGCHMDemoData", quietly = TRUE)) {
   data(TCGA.GBM.EXPR, package = "NGCHMDemoData")
    prc <- prcomp(TCGA.GBM.EXPR[1:50, 1:50])
    hm <- chmNew("gbm", TCGA.GBM.EXPR[1:50, 1:50])
    hm <- chmAddPCA(hm, "column", prc)
  }
  # Small example not requiring NGCHMDemoData
  matrix <- matrix(rnorm(100),
    nrow = 10, ncol = 10,
    dimnames = list(paste0("r", 1:10), paste0("c", 1:10))
  )
  prc <- prcomp(matrix)
  hm <- chmNew("Demo PCA", matrix)
  hm <- chmAddPCA(hm, "column", prc)
} # }
```
