# Create a new NGCHM.

This function creates a Next Generation Clustered Heat Map (NGCHM)
object in memory. Additional parameters will be added to the new NGCHM
(see chmAdd). The bare NGCHM needs at least one data layer added to it
before it can be compiled. This function requires **git** to be
installed.

## Usage

``` r
chmNew(
  name,
  ...,
  rowOrder = chmDefaultRowOrder,
  rowDist = "correlation",
  rowAgglom = "ward.D2",
  colOrder = chmDefaultColOrder,
  colDist = "correlation",
  colAgglom = "ward.D2",
  rowAxisType = NULL,
  colAxisType = NULL,
  rowCovariates = NULL,
  colCovariates = NULL,
  format = "original",
  rowGapLocations = NULL,
  rowGapWidth = 5,
  colGapLocations = NULL,
  colGapWidth = 5,
  panel_configuration = default_panel_configuration(),
  overview = c(),
  rowDisplayLength = 20,
  colDisplayLength = 20
)
```

## Arguments

- name:

  The name under which the NGCHM will be saved to the NGCHM server.

- ...:

  Zero or more initial objects to include in the NGCHM (see chmAdd).

- rowOrder:

  A vector, dendrogram, or function specifying the CHM row order.

- rowDist:

  Distance method to use by default RowOrder

- rowAgglom:

  Agglomeration method to use by default RowOrder

- colOrder:

  A vector, dendrogram, or function specifying the CHM column order.

- colDist:

  Distance method to use by default ColOrder

- colAgglom:

  Agglomeration method to use by default ColOrder

- rowAxisType:

  The type(s) of the row labels (default: None).

- colAxisType:

  The type(s) of the column labels (default: None).

- rowCovariates:

  Covariate(Bar)(s) to add to the rows (default: None).

- colCovariates:

  Covariate(Bar)(s) to add to the columns (default: None).

- format:

  The format of NGCHM to produce (default: 'original').

- rowGapLocations:

  Locations for row gaps. Specify as a list of integers or
  [`chmTreeGaps()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmTreeGaps.md)
  function.

- rowGapWidth:

  Width of row gaps (default: 5 rows)

- colGapLocations:

  Locations for col gaps. Specify as a list of integers or
  [`chmTreeGaps()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmTreeGaps.md)
  function.

- colGapWidth:

  Width of col gaps (default: 5 cols)

- panel_configuration:

  The configuration of the panels in the NGCHM (default:
  default_panel_configuration()).

- overview:

  The format(s) of overview image(s) to create (default: None).

- rowDisplayLength:

  The number of characters to display in NGCHM row labels (default: 20).

- colDisplayLength:

  The number of characters to display in NGCHM column labels (default:
  20).

## Value

An object of class ngchm

## See also

[ngchm](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchm-class.md)

[ngchmServer](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmServer-class.md)

[`chmAdd()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAdd-method.md)

[`chmAddAxisType()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddAxisType-method.md)

[`chmAddCovariateBar()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddCovariateBar-method.md)

[`chmAddProperty()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddProperty-method.md)

[`chmAddOverview()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddOverview-method.md)

[`chmInstall()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmInstall-method.md)

[`chmExportToFile()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmExportToFile-method.md)

[`chmExportToPDF()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmExportToPDF-method.md)

[`chmExportToHTML()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmExportToHTML-method.md)

## Examples

``` r
mychm <- chmNew("test_chm")
mychm <- chmNew("test_chm", rowGapLocations = c(3, 5))
mychm <- chmNew("test_chm", rowGapLocations = chmTreeGaps(4))
mychm <- chmNew("test_chm", rowGapWidth = 3)
```
