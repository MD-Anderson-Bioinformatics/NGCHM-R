# Pane Class for NG-CHM Layout

A pane represents a single viewing area in the NG-CHM panel layout
system.

## Details

Panes are the leaf nodes in the panel layout tree structure. They are
contained within containers and can be configured as detail maps,
summary maps, or plugin panes.

## Slots

- `collapsed`:

  Logical indicating if the pane is collapsed

- `expanded`:

  Logical indicating if the pane is expanded

- `height`:

  Numeric value for pane height (as percentage, 0-100)

- `id`:

  Character string identifying the pane

- `width`:

  Numeric value for pane width (as percentage, 0-100)

## See also

- [container](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/container.md)
  for parent container class

- [panel_configuration](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/panel_configuration.md)
  for complete configuration

## Examples

``` r
# Create a basic pane
p1 <- pane(id = "pane1", width = 50, height = 100)
```
