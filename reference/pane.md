# Create a pane for NG-CHM layout

Creates a new pane object representing a single viewing area in the
NG-CHM panel layout system.

## Usage

``` r
pane(id = NA, width = 100, height = 100, collapsed = FALSE, expanded = FALSE)
```

## Arguments

- id:

  Character string identifying the pane. Must be specified, no default.

- width:

  Numeric value for pane width, expressed as a percentage (0-100).
  Default is 100.

- height:

  Numeric value for pane height, expressed as a percentage (0-100).
  Default is 100.

- collapsed:

  Logical indicating if the pane is collapsed. Default is FALSE.

- expanded:

  Logical indicating if the pane is expanded. Default is FALSE.

## Value

A new `pane` object with the specified properties

## See also

- [pane](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/pane-class.md)
  for class details

- [panel_configuration](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/panel_configuration.md)
  for using panes in configurations

## Examples

``` r
# Create a basic pane
p1 <- pane(id = "pane1")

# Create a half-width pane
p2 <- pane(id = "pane2", width = 50)

# Create an expanded pane
p3 <- pane(id = "pane3", expanded = TRUE)
```
