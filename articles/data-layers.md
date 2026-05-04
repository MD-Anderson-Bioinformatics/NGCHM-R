# Data Layers

In addition to specifying color maps (see [Color
Maps](https://md-anderson-bioinformatics.github.io/NGCHM-R/articles/color-maps.md)),
data layers allow for displaying multiple data sets in the same NG-CHM.
This vignette describes how to create an NG-CHM with multiple data
layers.

These examples build on the setup from [Getting
Started](https://md-anderson-bioinformatics.github.io/NGCHM-R/articles/getting-started.md)

## Data Layers

The function
[`chmNewDataLayer()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewDataLayer.md)
creates a data layer. The first argument is the desired name of the data
layer, the second argument is the matrix of data, and the third argument
is the color map. The example below creates a color map and a data layer
for the TCGA BRCA Expression data. See [Color
Maps](https://md-anderson-bioinformatics.github.io/NGCHM-R/articles/color-maps.md)
for more information on creating color maps.

``` r

dataColorMap <- chmNewColorMap(c(6.4, 10, 14), c("mediumblue", "snow", "firebrick"))
dataLayer <- chmNewDataLayer("Unadjusted", matrix_data, dataColorMap)
```

For a second data layer, the data is row centered. The code block below
row-centers the data, creates a color map, and creates a second data
layer for the row-centered data.

``` r

rowCenteredData <- t(scale(t(matrix_data)))
rowCenteredColorMap <- chmNewColorMap(c(-2, 0, 2), c("#9933ff", "#f0f0f0", "#228B22"))
rowCenteredLayer <- chmNewDataLayer("Row-Centered", rowCenteredData, rowCenteredColorMap)
```

The NG-CHM can then be created with both data layers:

``` r

hm <- chmNew("TCGA BRCA Expression", dataLayer, rowCenteredLayer)
```

[Back to top](#)

## Resulting NG-CHM

Below is the full code block and resulting NG-CHM.

``` r

library(NGCHMDemoData)
library(NGCHMSupportFiles)
library(NGCHM)
matrix_data_file <- system.file("extdata", "TCGA.BRCA.Expression.csv", package = "NGCHMDemoData")
matrix_data <- as.matrix(read.csv(matrix_data_file, header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE))
covariate_data_file <- system.file("extdata", "TCGA.BRCA.TP53Mutation.csv", package = "NGCHMDemoData")
covariate_data <- as.matrix(read.csv(covariate_data_file, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE))
covariate_vector <- as.vector(covariate_data) # create vector of mutation data
names(covariate_vector) <- rownames(covariate_data) # set the names
dataColorMap <- chmNewColorMap(c(6.4, 10, 14), c("mediumblue", "snow", "firebrick"))
dataLayer <- chmNewDataLayer("Unadjusted", matrix_data, dataColorMap)
rowCenteredData <- t(scale(t(matrix_data)))
rowCenteredColorMap <- chmNewColorMap(c(-2, 0, 2), c("#9933ff", "#f0f0f0", "#228B22"))
rowCenteredLayer <- chmNewDataLayer("Row-Centered", rowCenteredData, rowCenteredColorMap)
hm <- chmNew("TCGA BRCA Expression", dataLayer, rowCenteredLayer)
chmExportToHTML(hm, "datalayers.html", overwrite = TRUE)
htmltools::tags$iframe(src = "datalayers.html", width = "100%", height = 700)
```

![](data:image/svg+xml;base64,PHN2ZyBzdHlsZT0iZGlzcGxheTpub25lOyI+PHN5bWJvbCBpZD0iaWNvbi1sYXllcnMiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgZmlsbD0iY3VycmVudENvbG9yIiBjbGFzcz0iYmkgYmktbGF5ZXJzIiB2aWV3Ym94PSIwIDAgMTYgMTYiPjxwYXRoIGQ9Ik04LjIzNSAxLjU1OWEuNS41IDAgMCAwLS40NyAwbC03LjUgNGEuNS41IDAgMCAwIDAgLjg4MkwzLjE4OCA4IC4yNjQgOS41NTlhLjUuNSAwIDAgMCAwIC44ODJsNy41IDRhLjUuNSAwIDAgMCAuNDcgMGw3LjUtNGEuNS41IDAgMCAwIDAtLjg4MkwxMi44MTMgOGwyLjkyMi0xLjU1OWEuNS41IDAgMCAwIDAtLjg4MmwtNy41LTR6bTMuNTE1IDcuMDA4TDE0LjQzOCAxMCA4IDEzLjQzMyAxLjU2MiAxMCA0LjI1IDguNTY3bDMuNTE1IDEuODc0YS41LjUgMCAwIDAgLjQ3IDBsMy41MTUtMS44NzR6TTggOS40MzMgMS41NjIgNiA4IDIuNTY3IDE0LjQzOCA2IDggOS40MzN6IiAvPjwvc3ltYm9sPjwvc3ZnPg==)

The resulting NG-CHM has the two data layers available. To toggle
between layers, click the
![](data:image/svg+xml;base64,PHN2ZyBzdHlsZT0id2lkdGg6MTZweDtoZWlnaHQ6MTZweDt2ZXJ0aWNhbC1hbGlnbjptaWRkbGU7IiB2aWV3Ym94PSIwIDAgMTYgMTYiPjx1c2UgeGxpbms6aHJlZj0iI2ljb24tbGF5ZXJzIiAvPjwvc3ZnPg==)
icon (the first of the 4 buttons on the upper right side).

[Back to top](#)
