# Create a new covariate Bar for a NGCHM

This function creates a new covariate bar suitable for adding to a Next
Generation Clustered Heat Map.

## Usage

``` r
chmNewCovariateBar(
  covar,
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

- covar:

  The covariate to be displayed in the bar.

- display:

  Whether the covariate bar will be "hidden" or "visible" (default).

- thickness:

  The thickness of the covariate bar in pixels. (Default 10).

- merge:

  Algorithm for merging covariates when necessary ("average",
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

[`chmAddCovariateBar()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddCovariateBar-method.md)

## Examples

``` r
bar.data <- ifelse(rnorm(1000) < 0, "negative", "non-negative")
names(bar.data) <- sprintf("Sample%d", 1:length(bar.data))
bar.colors <- chmNewColorMap(c("negative", "non-negative"),
  c("white", "black"),
  missing.color = "red"
)
covar <- chmNewCovariate("Group", bar.data, bar.colors, "discrete")
bar <- chmNewCovariateBar(covar)
```
