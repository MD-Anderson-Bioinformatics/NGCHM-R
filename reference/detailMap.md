# Create a detail map pane

Creates a new detail map object that provides a zoomed-in view of a
portion of the heatmap with configurable display settings.

## Usage

``` r
detailMap(
  id = NA,
  currentCol = 1,
  currentRow = 1,
  colZoomLevel = 1,
  dataBoxHeight = 42,
  dataBoxWidth = 42,
  dataPerCol = 12,
  dataPerRow = 12,
  mode = "NORMAL",
  rowZoomLevel = 1,
  selectedIsDendrogram = FALSE,
  selectedStart = 0,
  selectedStop = 0,
  version = "P",
  versionNumber = 1
)
```

## Arguments

- id:

  Character string identifying the detail map pane. Must be specified,
  no default.

- currentCol:

  Integer. Current column position in heatmap (must be \> 0). Default is
  1.

- currentRow:

  Integer. Current row position in heatmap (must be \> 0). Default is 1.

- colZoomLevel:

  Numeric. Column zoom level between 0 and 1. Default is 1.

- dataBoxHeight:

  Integer. Height of each data box. Must be one of the predefined sizes.
  Default is 42.

- dataBoxWidth:

  Integer. Width of each data box. Must be one of the predefined sizes.
  Default is 42.

- dataPerCol:

  Integer. Number of rows displayed in detail map (must be \> 1).
  Default is 12.

- dataPerRow:

  Integer. Number of columns displayed in detail map (must be \> 1).
  Default is 12.

- mode:

  Character. View mode: "NORMAL", "RIBBONH", or "RIBBONV". Default is
  "NORMAL".

- rowZoomLevel:

  Numeric. Row zoom level between 0 and 1. Default is 1.

- selectedIsDendrogram:

  Logical. Whether selection is in dendrogram. Default is FALSE.

- selectedStart:

  Integer. Start position of selection (\>= 0). Default is 0.

- selectedStop:

  Integer. End position of selection (\>= 0). Default is 0.

- version:

  Character. Map version: "P" for primary, "S" for secondary. Default is
  "P".

- versionNumber:

  Integer. Detail map number for multiple detail panes. Default is 1.

## Value

A new `detailMap` object with the specified properties

## See also

- [detailMap](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/detailMap-class.md)
  for class details

- [panel_configuration](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/panel_configuration.md)
  for using detail maps in configurations

## Examples

``` r
# Create a basic detail map
detail <- detailMap(id = "pane1")

# Create a detail map with custom zoom and box size
detail2 <- detailMap(
  id = "pane2",
  colZoomLevel = 0.5,
  rowZoomLevel = 0.5,
  dataBoxHeight = 84,
  dataBoxWidth = 84
)
```
