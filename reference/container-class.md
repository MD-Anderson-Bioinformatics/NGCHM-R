# Container Class for Panel Layout

The container class is part of the panel layout system in NG-CHM. It is
used to organize panes in a hierarchical structure. Each container can
be either vertical or horizontal, and can contain multiple children
(panes or other containers).

## Slots

- `children`:

  A list of child objects (panes or other containers)

- `height`:

  Numeric value for container height (as percentage, 0-100)

- `id`:

  Character string identifying the container (e.g. "ngChmContainer1")

- `vertical`:

  Logical indicating container is vertical (TRUE) or horizontal (FALSE)

- `width`:

  Numeric value for container width (as percentage, 0-100)

## See also

- [pane](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/pane.md)
  for leaf nodes in the layout tree
