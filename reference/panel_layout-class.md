# Panel Layout Class for NG-CHM

A specialized container class that represents the top-top-level layout
structure in the NG-CHM panel configuration. Inherits from the container
class, but provides a custom JSON serialization that enforces the
required top-top-level container structure.

## Details

The panel_layout class creates the required two-level container
hierarchy:

- A top-top-level container with id "ngChmContainer"

- A child container that holds the actual panel layout structure

This class inherits all slots from the container class but provides a
specialized JSON serialization method to ensure proper formatting for
the NG-CHM viewer.

## See also

- [container](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/container.md)
  for the parent class

- [panel_configuration](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/panel_configuration.md)
  for the complete panel configuration
