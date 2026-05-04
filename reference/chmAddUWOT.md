# Add UWOT::UMAP coordinates to an NG-CHM.

Add UWOT::UMAP coordinates as hidden covariate bars to an axis of an
NG-CHM. One hidden covariate bar is added for each UMAP coordinate.
Coordinates have names 'BASENAME.coordinate.N', where BASENAME is
specified by the parameter basename (default UMAP) and N ranges from 1
to the number of added covariate bars.

## Usage

``` r
chmAddUWOT(hm, axis, uwot, pointIds, basename = "UMAP")
```

## Arguments

- hm:

  The NGCHM to add the coordinates to.

- axis:

  The NGCHM axis ("row" or "column") to add the coordinates to.

- uwot:

  UMAP coordinates (output of
  [uwot::umap()](https://CRAN.R-project.org/package=uwot)) for the
  specified NGCHM axis.

- pointIds:

  The NGCHM names for the data points in uwot

- basename:

  The prefix to use for the coordinate names.

## Value

The NGCHM with added coordinates.

## Details

pointIds is required because
[uwot::umap()](https://CRAN.R-project.org/package=uwot) does not
preserve the rownames of the data matrix it was applied to. Their values
must match those on that axis of the NGCHM, but their order must match
those in the data matrix passed to
[uwot::umap()](https://CRAN.R-project.org/package=uwot).

## See also

[`chmAddPCA()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddPCA.md)

[`chmAddTSNE()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddTSNE.md)

[`chmAddUMAP()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddUMAP.md)

[`chmAddReducedDim()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddReducedDim.md)

## Examples

``` r
# Examples using `chmNew()` require git to be installed.
if (FALSE) { # \dontrun{
  # If the NGCHMDemoData package is installed, use it to demo usage
  if (requireNamespace("NGCHMDemoData", quietly = TRUE)) {
    data(TCGA.GBM.EXPR, package = "NGCHMDemoData")
    umc <- uwot::umap(t(TCGA.GBM.EXPR[1:50, 1:50]))
    hm <- chmNew("gbm", TCGA.GBM.EXPR[1:50, 1:50])
    hm <- chmAddUWOT(hm, "column", umc, colnames(TCGA.GBM.EXPR[1:50, 1:50]))
  }
  # Small example not requiring NGCHMDemoData
  matrix <- matrix(rnorm(100),
    nrow = 10, ncol = 10,
    dimnames = list(paste0("r", 1:10), paste0("c", 1:10))
  )
  umc <- uwot::umap(t(matrix), n_neighbors = 8)
  hm <- chmNew("Demo UMAP", matrix)
  hm <- chmAddUWOT(hm, "column", umc, colnames(matrix))
} # }
```
