# Panel Configuration Class for NG-CHM

This configuration appears in the `panel_configuration` key of the
mapConfig.json file under keys:

- `panel_layout`: The hierarchical structure of containers and panes.
  The `panes_list` slot is converted to this structure.

- Individual pane configurations (e.g., `pane1`, `pane2`) containing
  pane-specific settings. The `pane_types` slot is converted to these
  individual pane configurations.

## Slots

- `panes_list`:

  A list containing pane objects and sizes information.

- `pane_types`:

  A named list mapping pane IDs to their configurations.

- `selections`:

  A list of lists of row and column labels.

## See also

- [detailMap](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/detailMap.md)
  for detail map configuration

- [summaryMap](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/summaryMap.md)
  for summary map configuration

- [pluginPane](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/pluginPane.md)
  for plugin pane configuration
