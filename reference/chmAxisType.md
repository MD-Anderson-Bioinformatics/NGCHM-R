# Create a new AxisType for adding to an ngchmAxis.

This function creates a new AxisType for adding to an ngchmAxis.

## Usage

``` r
chmAxisType(typename, func)
```

## Arguments

- typename:

  The name of the axis type to be created. This should be a single
  character string.

- func:

  The function to be used for getting label values. If not provided, the
  default 'getLabelValue' function is used. If a character string is
  provided, it is assumed to be the name of a function and is retrieved
  using 'chmGetFunction'. If a function is provided, it is checked to be
  of class 'ngchmJS'.

## Value

An object of class 'ngchmAxisType' representing the newly created axis
type.

## See also

[`chmAxis()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAxis.md)
