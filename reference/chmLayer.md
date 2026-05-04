# Get a specified Data Layer from an NG-CHM.

This function returns a Data Layer contained in a Next Generation
Clustered Heat Map.

## Usage

``` r
chmLayer(hm, label)
```

## Arguments

- hm:

  The NG-CHM object to get the data layer from.

- label:

  The name or index of the data layer to get. If a name, return the
  layer with that name. If no layer with that name exists or if the
  index is out of range, return NULL.

## Value

An object of class ngchmLayer or NULL.

## See also

[ngchmLayer](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmLayer-class.md)

## Examples

``` r
# Examples using `chmNew()` require git to be installed and available.
if (FALSE) { # \dontrun{
 # If the NGCHMDemoData package is installed, use it to create an example usage
 if (requireNamespace("NGCHMDemoData", quietly = TRUE)) {
    # Create example NGCHM
    data(TCGA.GBM.Demo, package = "NGCHMDemoData")
    matrix <- TCGA.GBM.ExpressionData[1:50, 1:50]
    hm <- chmNew("New Heat Map") + chmNewDataLayer("my layer", matrix)
    layer <- chmLayer(hm, "my layer")
    same_layer <- chmLayer(hm, 1)
  }
  # Small example not requiring NGCHMDemoData
  matrix <- matrix(rnorm(100),
    nrow = 10, ncol = 10,
    dimnames = list(paste0("r", 1:10), paste0("c", 1:10))
  )
  hm <- chmNew("New Heat Map") + chmNewDataLayer("my layer", matrix)
  layer <- chmLayer(hm, "my layer")
  same_layer <- chmLayer(hm, 1)
} # }
```
