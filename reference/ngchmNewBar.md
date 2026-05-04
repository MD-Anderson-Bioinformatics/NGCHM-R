# Create a new Classification Bar for a NGCHM

This function is deprecated and will be removed in a future version.
Please use chmNewCovariateBar. This function creates a new
Classification Bar suitable for adding to a Next Generation Clustered
Heat Map.

## Usage

``` r
ngchmNewBar(
  label,
  type,
  data,
  colors = NULL,
  display = "visible",
  thickness = as.integer(10),
  merge,
  barType,
  loBound,
  hiBound,
  fgColor,
  bgColor
)
```

## Arguments

- label:

  The name by which the classification bar will be known.

- type:

  The string "discrete" or the string "continuous".

- data:

  A vector of the data to be displayed in the classification bar.
  names(data) must be defined.

- colors:

  A color map specifying how the data should be rendered.

- display:

  Whether the classification bar will be "hidden" or "visible"
  (default).

- thickness:

  The thickness of the classification bar in pixels. (Default 10).

- merge:

  Algorithm for merging classifications when necessary ("average",
  "peakColor", "specialColor", or "mostCommon").

- barType:

  Type of covariate bar ("color_plot", "scatter_plot", "bar_plot").
  Default "color_plot".

- loBound:

  Low bound for bar and scatter plots. Default minimum data value.

- hiBound:

  High bound for bar and scatter plots. Default maximum data value.

- fgColor:

  Foreground color for bar and scatter plots. Default black.

- bgColor:

  Background color for bar and scatter plots. Default white.

## Value

An object of class ngchmBar

## See also

[ngchmBar](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmBar-class.md)

[`chmNewColorMap()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewColorMap.md)

[`chmNewCovariateBar()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewCovariateBar.md)

[`chmAddCovariateBar()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddCovariateBar-method.md)
