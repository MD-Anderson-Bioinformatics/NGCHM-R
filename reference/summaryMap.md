# Create a summary map pane

Creates a new summary map object for use in NG-CHM panel configurations.

## Usage

``` r
summaryMap(id = NA)
```

## Arguments

- id:

  Character string identifying the summary map pane. Must be specified,
  no default.

## Value

A new `summaryMap` object with the specified ID

## See also

- [summaryMap](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/summaryMap-class.md)
  for class details

- [panel_configuration](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/panel_configuration.md)
  for using summary maps in configurations

- [detailMap](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/detailMap.md)
  for detail map panes

## Examples

``` r
# Create a summary map pane
summary <- summaryMap(id = "pane2")
```
