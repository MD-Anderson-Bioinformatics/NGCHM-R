# Create a new Axis for adding to an NG-CHM.

This function creates a new Axis for adding to a Next Generation
Clustered Heat Map. You can specify any axis name here, but chmAdd only
accepts row, column, and both.

## Usage

``` r
chmAxis(axis, ...)
```

## Arguments

- axis:

  The name of the axis

- ...:

  Objects to add to the axis

## Value

An object of class 'ngchmAxis' representing the newly created axis.

## See also

[`chmAdd()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAdd-method.md)

## Examples

``` r
x_axis <- chmAxis('row')
y_axis <- chmAxis('col')
```
