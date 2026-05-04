# Create a new Data Layer for a NGCHM.

This function creates a new Data Layer suitable for adding to a Next
Generation Clustered Heat Map.

## Usage

``` r
chmNewDataLayer(label, data, colors, summarizationMethod, cuts_color)
```

## Arguments

- label:

  The name under which the data layer will be displayed to the user.

- data:

  A matrix containing the data to display. Must have rownames and
  colnames.

- colors:

  A color map specifying how the data should be rendered. If omitted or
  NULL, a default green-black-red color map will be estimated from the
  data.

- summarizationMethod:

  The method to use when summarizing multiple data points per pixel.
  Possible values are average (default), sample, and mode.

- cuts_color:

  color of cuts

## Value

An object of class ngchmLayer

## See also

[ngchmLayer](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmLayer-class.md)

[`chmNewColorMap()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewColorMap.md)

[`chmAddLayer()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddLayer-method.md)

## Examples

``` r
noise <- matrix(runif(1000) + runif(1000 * 1000), nrow = 1000)
rownames(noise) <- sprintf("Row%d", 1:nrow(noise))
colnames(noise) <- sprintf("Col%d", 1:ncol(noise))
noise.colors <- chmNewColorMap(c(0, 1, 2),
  c("green", "black", "red"),
  missing.color = "yellow"
)
layer <- chmNewDataLayer("Noisy Data", noise, noise.colors)
```
