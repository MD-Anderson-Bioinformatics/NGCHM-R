# Create a plugin pane for NG-CHM layout

Creates a new plugin pane object that represents a specialized
visualization component in the NG-CHM panel layout system, such as
scatter plots or bar charts.

## Usage

``` r
pluginPane(id = NA, pluginName = NA)
```

## Arguments

- id:

  Character string identifying the plugin pane. Must be specified, no
  default.

- pluginName:

  Character string specifying the plugin type (e.g., "2D ScatterPlot:
  UMAP (column)"). Must be specified, no default.

## Value

A new `pluginPane` object with the specified properties

## See also

- [pluginPane](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/pluginPane-class.md)
  for class details

- [panel_configuration](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/panel_configuration.md)
  for using plugin panes in configurations

## Examples

``` r
# Create a UMAP scatter plot plugin pane
plugin <- pluginPane(
  id = "pane3",
  pluginName = "2D ScatterPlot: UMAP (column)"
)
```
