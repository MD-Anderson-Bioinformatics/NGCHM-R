# Constructor for panel_configuration

Creates a new panel configuration object that combines pane layout
information with individual pane specifications.

## Usage

``` r
panel_configuration(
  panes_list,
  pane_types,
  selections = list(row = list(), col = list())
)
```

## Arguments

- panes_list:

  A list containing pane objects and sizes information, where sizes is
  expressed as a percentage. For example, this panes_list format:  
    


               list(pane(id="pane1"),
                    pane(id="pane2"),
                    sizes = c(40, 60))
             

  will produce a layout like the one below, where `pane1` accounts for
  40% of the horizontal space and `pane2` accounts for 60% of the
  horizontal space:


                            |
                      pane1 | pane2
                            |
                

  This panes_list format:


                  list(pane(id="pane1"),
                       list(pane(id="pane2"),
                            pane(id="pane3"),
                            sizes = c(30, 70)), # for pane2, pane3
                  sizes = c(50, 50))  # for pane1 & container of pane2, pane3
                

  will produce a layout like the one below, where `pane2` accounts for
  30% of the vertical space and `pane3` accounts for 70% of the vertical
  space:


                              |  pane2
                       pane1  | -------
                              |  pane3
             

- pane_types:

  A named list mapping pane IDs to their configurations. Each element
  must be a detailMap, summaryMap, or pluginPane object. For example,
  the default two-pane layout is a detailMap and a summaryMap:


                  pane_types = list(
                         pane1 = detailMap(id = "pane1"),
                         pane2 = summaryMap(id = "pane2")
                  )
                

  For example, a three-pane layout with a detail map, a summary map, and
  a plugin pane:


                  pane_types = list(
                         pane1 = detailMap(id = "pane1"),
                         pane2 = summaryMap(id = "pane2"),
                         pane3 = pluginPane(id = "pane3",
                            pluginName = "2D ScatterPlot: UMAP (column)")
                  )
                

- selections:

  A list of lists of row and column labels. For example, to select rows
  "r1" and "r4" and columns "c2" and "c5":


                  selections = list(row = c("r1", "r4"), col = c("c2", "c5"))
                

## Value

A new `panel_configuration` object

## See also

- [panel_configuration](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/panel_configuration-class.md)
  for class details

- [pane](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/pane.md)
  for creating pane objects

- [detailMap](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/detailMap.md)
  for detail map configuration

- [summaryMap](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/summaryMap.md)
  for summary map configuration

- [pluginPane](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/pluginPane.md)
  for plugin pane configuration

## Examples

``` r
matrix <- matrix(rnorm(100),
  nrow = 10, ncol = 10,
  dimnames = list(paste0("r", 1:10), paste0("c", 1:10))
)
# Create a three-pane layout with a detail map, a summary map, and a plugin pane:
pane1 <- pane(id = "pane1")
pane2 <- pane(id = "pane2")
pane3 <- pane(id = "pane3")
panes_list <- list(pane1, list(pane2, pane3, sizes = c(30, 70)), sizes = c(50, 50))

# Create pane configuration for each pane
pane_types <- list(
  pane1 = detailMap(id = "pane1"),
  pane2 = summaryMap(id = "pane2"),
  pane3 = pluginPane(id = "pane3", pluginName = "2D ScatterPlot: UMAP (column)")
)

# Select the 1st and 4th rows and 2nd and 5th columns:
selections <- list(row = c("r1", "r4"), col = c("c2", "c5"))

# Create panel configuration
config <- panel_configuration(panes_list, pane_types, selections = selections)
# Create a new NG-CHM heatmap with this panel configuration
hm <- chmNew("three-panel-ngchm", matrix, panel_configuration = config)
```
