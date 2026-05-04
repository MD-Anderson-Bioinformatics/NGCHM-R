# Add a Layer to a NGCHM.

Add a Layer to a Next Generation Clustered Heat Map (NGCHM) and return
the extended CHM. A CHM requires at least one Layer. The first layer
added to a NGCHM becomes the primary layer. The second layer added to a
NGCHM, if any, becomes the secondary (flicker) layer. Currently at most
two layers can be added to a NGCHM.

## Usage

``` r
chmAddLayer(chm, layer)

# S4 method for class 'ngchm,ngchmLayer'
chmAddLayer(chm, layer)

# S4 method for class 'ngchm,matrix'
chmAddLayer(chm, layer)
```

## Arguments

- chm:

  The chm to add the layer to.

- layer:

  The layer to add to the chm.

## Value

The extended chm.

## See also

[`chmNewDataLayer()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewDataLayer.md)

[ngchmLayer](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmLayer-class.md)
