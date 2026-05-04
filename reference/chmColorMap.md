# Get the color map of an NG-CHM object.

Get the color map of an NG-CHM object.

## Usage

``` r
chmColorMap(x)
```

## Arguments

- x:

  The NG-CHM object to get the color map of. Can be:

  - An object of class ngchmLayer

  - An object of class ngchmBar

  - An object of class ngchmCovariate

## Value

An ngchmColormap

## See also

[chmNewColorMap](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewColorMap.md)

## Examples

``` r
# If the NGCHMDemoData package is installed, use it to demo usage
if (requireNamespace("NGCHMDemoData", quietly = TRUE)) {
  data(TCGA.GBM.EXPR, package = "NGCHMDemoData")
  colormap <- chmColorMap(chmNewDataLayer("New layer", TCGA.GBM.EXPR[1:3, 1:3]))
}
matrix <- matrix(rnorm(100),
  nrow = 10, ncol = 10,
  dimnames = list(paste0("r", 1:10), paste0("c", 1:10))
)
colormap <- chmColorMap(chmNewDataLayer("New layer", matrix))
```
