# Get the colors of an ngchmColormap, ngchmLayer, ngchmBar, or ngchmCovariate.

Get the colors of an ngchmColormap, ngchmLayer, ngchmBar, or
ngchmCovariate.

## Usage

``` r
chmColors(x)
```

## Arguments

- x:

  The object to get the colors of.

## Value

A character string vector of the map colors.

## See also

[ngchm](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchm-class.md)

## Examples

``` r
# If the NGCHMDemoData package is installed, use it to demo usage
if (requireNamespace("NGCHMDemoData", quietly = TRUE)) {
  data(TCGA.GBM.EXPR, package = "NGCHMDemoData")
  colors <- chmColors(chmNewDataLayer("GBM Expression", TCGA.GBM.EXPR[1:50, 1:50]))
}
# Small example not requiring NGCHMDemoData
matrix <- matrix(rnorm(100),
  nrow = 10, ncol = 10,
  dimnames = list(paste0("r", 1:10), paste0("c", 1:10))
)
colors <- chmColors(chmNewDataLayer("my layer", matrix))
```
