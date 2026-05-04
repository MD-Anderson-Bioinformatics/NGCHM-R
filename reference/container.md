# Create a container object for panel layout

Creates a new container object that can hold panes or other containers
in the NG-CHM panel layout system. A container organizes its children
either vertically or horizontally and specifies their dimensions as
percentages.

## Usage

``` r
container(
  children = list(),
  height = 100,
  id = NA,
  vertical = FALSE,
  width = 100
)
```

## Arguments

- children:

  A list of child objects (panes or other containers). Default is an
  empty list.

- height:

  Numeric value for container height, expressed as a percentage (0-100).
  Default is 100.

- id:

  Character string identifying the container. Must be specified, no
  default.

- vertical:

  Logical indicating if children are arranged vertically (TRUE) or
  horizontally (FALSE). Default is FALSE.

- width:

  Numeric value for container width, expressed as a percentage (0-100).
  Default is 100.

## Value

A new `container` object with the specified properties.
