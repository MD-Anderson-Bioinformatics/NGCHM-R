# Register a predefined Javascript function for use in NGCHM Axis menus.

This function registers a Javascript function that will be automatically
added to the appropriate axis menu(s) when building a Next Generation
Clustered Heat Map for axes that match the function's axis type. This
function is intended for use by NGCHM system developers.

## Usage

``` r
chmRegisterAxisFunction(type, label, fn)
```

## Arguments

- type:

  The axis type required by this function.

- label:

  The name of the axis menu entry to be used for this function.

- fn:

  The Javascript function to register.

## Value

None. This function is used for its side effects of registering a new
axis function.

## See also

[`chmAddAxisType()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddAxisType-method.md)

[`chmRegisterMatrixFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRegisterMatrixFunction.md)

[`chmRegisterTypeMapper()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRegisterTypeMapper.md)

[`chmNewFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewFunction.md)
