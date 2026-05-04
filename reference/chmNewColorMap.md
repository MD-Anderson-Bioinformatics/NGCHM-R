# Create a new Color Map for use in constructing a NGCHM

This function creates a new Color Map suitable for use in constructing
Data Layers and Covariates in Next Generation Clustered Heat Maps. Color
maps can be used in both discrete and continuous contents. In a discrete
context, values specifies the properties of series. In a continuous
context, values specifies the break points.

## Usage

``` r
chmNewColorMap(
  values,
  colors = NULL,
  names = NULL,
  shapes = NULL,
  zs = NULL,
  type = "linear",
  missing.color = NULL,
  palette = NULL
)
```

## Arguments

- values:

  A vector specifying the series / break points for which the following
  colors are defined, or a data matrix.

- colors:

  Either a string vector specifying the color to use for each series /
  break point, or a single integer.

- names:

  A string vector specifying 'human-readable' names for each series /
  break point.

- shapes:

  A string vector specifying the shape to use for each series.

- zs:

  A numeric vector specifying the z order to use for each series.

- type:

  The string "linear" (default) or "quantile" (or unique abbreviation
  thereof).

- missing.color:

  A string specifying the color to use for missing data.

- palette:

  A function(n) that returns a vector of n colors.

## Value

An object of class ngchmColormap

## Details

If values is a matrix, the function will estimate a suitable sequence of
color break points. For a quantile color map, the matrix data is
ignored. For a linear color map, it will use equispaced values between a
low value and a high value. The low value is the median of the minima of
each row in the matrix, and the high value is the median of the row
maxima. If the low and high values have different signs, the values will
be symmetric about zero.

## See also

[ngchmColormap](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmColormap-class.md)

[`chmNewDataLayer()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewDataLayer.md)

[`chmNewCovariateBar()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewCovariateBar.md)

## Examples

``` r
noise.colors <- chmNewColorMap(c(0, 1, 2),
  c("green", "black", "red"),
  missing.color = "yellow"
)
bar.colors <- chmNewColorMap(c("small", "big"),
  c("#00FFFF", "#FF00FF"),
  type = "quantile"
)
```
