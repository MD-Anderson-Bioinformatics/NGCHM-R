# Set the colors of an ngchmColormap, ngchmLayer, ngchmBar, or ngchmCovariate.

Set the colors of an ngchmColormap, ngchmLayer, ngchmBar, or
ngchmCovariate.

## Usage

``` r
chmColors(x) <- value
```

## Arguments

- x:

  The NG-CHM object on which to set the colors.

- value:

  A character string vector of colors. The vector length must equal the
  number of data points in the color map.

## Value

The modified NG-CHM object.

## See also

[chmColors](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmColors.md)

## Examples

``` r
# If the NGCHMDemoData package is installed, use it to demo usage
if (requireNamespace("NGCHMDemoData", quietly = TRUE)) {
  data(TCGA.GBM.EXPR, package = "NGCHMDemoData")
  layer <- chmNewDataLayer("GBM Layer", TCGA.GBM.EXPR[1:50, 1:50])
  chmColors(layer) <- c("blue", "white", "red")
}
# Small example not requiring NGCHMDemoData
matrix <- matrix(rnorm(100),
  nrow = 10, ncol = 10,
  dimnames = list(paste0("r", 1:10), paste0("c", 1:10))
)
layer <- chmNewDataLayer("my layer", matrix)
chmColors(layer) <- c("blue", "white", "red")
```
