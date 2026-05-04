# Detail Map Class for NG-CHM Layout

Represents a detail map pane in the NG-CHM panel layout system. Detail
maps provide a zoomed-in view of a portion of the heatmap with
configurable display settings.

## Slots

- `currentCol`:

  Integer. Current column position in the heatmap (must be \> 0)

- `currentRow`:

  Integer. Current row position in the heatmap (must be \> 0)

- `colZoomLevel`:

  Numeric. Column zoom level between 0 and 1

- `dataBoxHeight`:

  Integer. Height of each data box. Must be one of the predefined sizes
  from NG-CHM Viewer

- `dataBoxWidth`:

  Integer. Width of each data box. Must be one of the predefined sizes
  from NG-CHM Viewer

- `dataPerCol`:

  Integer. Number of rows displayed in detail map (must be \> 1)

- `dataPerRow`:

  Integer. Number of columns displayed in detail map (must be \> 1)

- `id`:

  Character. Unique identifier for the pane

- `mode`:

  Character. View mode: "NORMAL", "RIBBONH", or "RIBBONV"

- `rowZoomLevel`:

  Numeric. Row zoom level between 0 and 1

- `selectedIsDendrogram`:

  Logical. Whether selection is in dendrogram

- `selectedStart`:

  Integer. Start position of selection (\>= 0)

- `selectedStop`:

  Integer. End position of selection (\>= 0)

- `version`:

  Character. Map version: "P" for primary, "S" for secondary

- `versionNumber`:

  Integer. Detail map number for multiple detail panes

## See also

- [panel_configuration](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/panel_configuration.md)
  for using detail maps in configurations

- [summaryMap](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/summaryMap.md)
  for summary map configuration
