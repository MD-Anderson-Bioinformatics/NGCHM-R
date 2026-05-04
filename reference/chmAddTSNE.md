# Add TSNE coordinates to an NG-CHM.

Add TSNE coordinates as hidden covariate bars to an axis of an NG-CHM.
One hidden covariate bar is added for each TSNE coordinate. Coordinates
have names 'BASENAME.coordinate.N', where BASENAME is specified by the
parameter basename (default TSNE) and N ranges from 1 to the number of
added covariate bars.

## Usage

``` r
chmAddTSNE(hm, axis, tsne, pointIds, basename = "TSNE")
```

## Arguments

- hm:

  The NGCHM to add the coordinates to

- axis:

  The NGCHM axis ("row" or "column") to add the coordinates to

- tsne:

  TSNE coordinates (output of
  [Rtsne::Rtsne()](https://CRAN.R-project.org/package=Rtsne)) for the
  specified NGCHM axis

- pointIds:

  The NGCHM names for the data points in tsne

- basename:

  The prefix to use for the coordinate names.

## Value

The NGCHM with added coordinates.

## Details

pointIds is required because
[Rtsne::Rtsne()](https://CRAN.R-project.org/package=Rtsne) does not
preserve the rownames of the data matrix it was applied to. Their values
must match those on that axis of the NGCHM, but their order must match
those in the data matrix passed to
[Rtsne::Rtsne()](https://CRAN.R-project.org/package=Rtsne).

## See also

[`chmAddPCA()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddPCA.md)

[`chmAddUMAP()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddUMAP.md)

[`chmAddUWOT()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddUWOT.md)

[`chmAddReducedDim()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddReducedDim.md)

## Examples

``` r
# Examples using  `chmNew()` require git to be installed.
if (FALSE) { # \dontrun{
  # If the NGCHMDemoData package is installed, use it to demo usage
  if (requireNamespace("NGCHMDemoData", quietly = TRUE)) {
    data(TCGA.GBM.EXPR, package = "NGCHMDemoData")
    mat <- TCGA.GBM.EXPR[1:10, 1:10]
    rtc <- Rtsne::Rtsne(t(mat), check_duplicates = FALSE, perplexity = 3)
    hm <- chmNew("gbm", mat)
    hm <- chmAddTSNE(hm, "column", rtc, colnames(mat))
  }
  # Small example not requiring NGCHMDemoData
  matrix <- matrix(rnorm(100),
    nrow = 10, ncol = 10,
    dimnames = list(paste0("r", 1:10), paste0("c", 1:10))
  )
  rtc <- Rtsne::Rtsne(t(matrix), check_duplicates = FALSE, perplexity = 3)
  hm <- chmNew("Demo TSNE", matrix)
  hm <- chmAddTSNE(hm, "column", rtc, colnames(matrix))
} # }
```
