# Add UMAP coordinates to an NG-CHM.

Add UMAP coordinates as hidden covariate bars to an axis of an NG-CHM.
One hidden covariate bar is added for each UMAP coordinate. Coordinates
have names 'BASENAME.coordinate.N', where BASENAME is specified by the
parameter basename (default UMAP) and N ranges from 1 to the number of
added covariate bars.

## Usage

``` r
chmAddUMAP(hm, axis, umap, basename = "UMAP")
```

## Arguments

- hm:

  The NGCHM to add the coordinates to.

- axis:

  The NGCHM axis ("row" or "column") to add the coordinates to.

- umap:

  UMAP coordinates (output of
  [umap::umap()](https://CRAN.R-project.org/package=umap)) for the
  specified NGCHM axis.

- basename:

  The prefix to use for the coordinate names.

## Value

The NGCHM with added coordinates.

## See also

[`chmAddPCA()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddPCA.md)

[`chmAddTSNE()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddTSNE.md)

[`chmAddUWOT()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddUWOT.md)

[`chmAddReducedDim()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddReducedDim.md)

## Examples

``` r
# Examples using `chmNew()` require git to be installed.
if (FALSE) { # \dontrun{
  # If the NGCHMDemoData package is installed, use it to demo usage
  if (requireNamespace("NGCHMDemoData", quietly = TRUE)) {
    data(TCGA.GBM.EXPR, package = "NGCHMDemoData")
    mat <- TCGA.GBM.EXPR[1:50, 1:50]
    umc <- umap::umap(t(mat))
    hm <- chmNew("gbm", mat)
    hm <- chmAddUMAP(hm, "column", umc)
  }
  # Small example not requiring NGCHMDemoData
  matrix <- matrix(rnorm(100),
    nrow = 10, ncol = 10,
    dimnames = list(paste0("r", 1:10), paste0("c", 1:10))
  )
  umc <- umap::umap(t(matrix), n_neighbors = 8)
  hm <- chmNew("Demo UMAP", matrix)
  hm <- chmAddUMAP(hm, "column", umc)
} # }
```
