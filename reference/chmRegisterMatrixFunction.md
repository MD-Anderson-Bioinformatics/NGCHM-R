# Register a predefined Javascript function for use in NGCHM Matrix menus.

This function registers a Javascript function that will be automatically
added to the matrix menu when building a Next Generation Clustered Heat
Map for matrices whose rows and columns match then function's axes
types. This function is intended for use by NGCHM system developers.

## Usage

``` r
chmRegisterMatrixFunction(rowtype, columntype, label, fn)
```

## Arguments

- rowtype:

  A character vector specifying the row type(s) of the matrix function.

- columntype:

  A character vector specifying the column type(s) of the matrix
  function.

- label:

  A single character string specifying the label of the matrix function.

- fn:

  The function to be registered. This can be either a function or a
  character string representing the name of a function.

## Value

None. This function is used for its side effects of registering a new
function in the NGCHM matrix menues.

## See also

[`chmAddAxisType()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddAxisType-method.md)

[`chmRegisterAxisFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRegisterAxisFunction.md)

[`chmRegisterTypeMapper()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRegisterTypeMapper.md)

[`chmNewFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewFunction.md)
