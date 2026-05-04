# Generate an overview image of the NGCHM when making it.

Generate an overview image of the NGCHM when making it. By default, the
system generates no default overview images. If only one of width or
height is specified, the other is calculated based on the aspect ratio
of the map.

## Usage

``` r
chmAddOverview(chm, format, width, height)

# S4 method for class 'ngchm,character,optNumeric,optNumeric'
chmAddOverview(chm, format, width, height)
```

## Arguments

- chm:

  The chm to add the overview to.

- format:

  The format of the overview ('pdf', 'png', or 'svg').

- width:

  The width of the overview.

- height:

  The height of the overview.

## Value

The extended chm.
