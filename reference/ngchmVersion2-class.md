# Class representing ngchmVersion2 object

Class representing ngchmVersion2 object

## Slots

- `name`:

  The name under which the NGCHM will be saved to the NGCHM server.

- `version`:

  Integer version number (default: 2)

- `format`:

  (default: "original")

- `uuid`:

  character

- `baggage`:

  optCharacter

- `inpDir`:

  character

- `outDir`:

  character

- `saveDir`:

  (default: tempdir())

- `propFile`:

  (default: "chm.properties")

- `layers`:

  List of data layers

- `colormaps`:

  Color map

- `rowMenu`:

  optList

- `colMenu`:

  optList

- `datasets`:

  optList

- `dialogs`:

  optList

- `tags`:

  optCharacter

- `elementMenu`:

  optList

- `rowTypeFunctions`:

  optList

- `colTypeFunctions`:

  optList

- `elementTypeFunctions`:

  optList

- `axisTypes`:

  optList

- `css`:

  optList

- `extrafiles`:

  optCharacter

- `extrascripts`:

  optCharacter

- `properties`:

  optList

- `overviews`:

  optList

- `javascript`:

  optList

- `rowOrder`:

  A vector, dendrogram, or function specifying the CHM row order

- `rowDist`:

  Distance method to use by default RowOrder. (default: "correlation",
  which is 1 minus the Pearson correlation among the rows.)

- `rowAgglom`:

  Agglomeration method to use by default RowOrder. Choices are those
  from stats::hclust. (default: "ward.D2")

- `colOrder`:

  A vector, dendrogram, or function specifying the CHM column order.

- `colDist`:

  Distance method to use by default ColOrder. (default: "correlation",
  which is 1 minus the Pearson correlation among the cols.)

- `colAgglom`:

  Agglomeration method to use by default ColOrder. Choices are those
  from stats::hclust. (default: "ward.D2")

- `rowOrderMethod`:

  character (default: "User")

- `colOrderMethod`:

  character (default: "User")

- `rowCutLocations`:

  Explicit list of row cut locations. If specified, rowTreeCuts is set
  to NULL.

- `rowTreeCuts`:

  Number of tree cuts for row. If specified, rowCutLocations is set to
  NULL.

- `rowCutWidth`:

  Width of row cuts (default: 5 rows)

- `rowTopItems`:

  optCharacter

- `rowDisplayLength`:

  optInteger

- `rowDisplayAbbreviation`:

  optCharacter

- `colCutLocations`:

  Explicit list of col cut locations. If specified, colTreeCuts is set
  to NULL.

- `colTreeCuts`:

  Number of tree cuts for col. If specified, colCutLocations is set to
  NULL.

- `colCutWidth`:

  Width of col cuts (defautl: 5 columns)

- `colTopItems`:

  optCharacter

- `colDisplayLength`:

  optInteger

- `colDisplayAbbreviation`:

  optCharacter

- `rowMeta`:

  optList

- `colMeta`:

  optList

- `rowCovariateBars`:

  optList

- `colCovariateBars`:

  optList

- `relatedLinks`:

  optList

- `relatedGroups`:

  optList

- `templates`:

  optList

- `width`:

  default: 500

- `height`:

  default: 500

- `panel_configuration`:

  panel_configuration
