# Register a predefined Javascript function for use in NGCHM menus.

This function registers a Javascript function that can be used when
building a Next Generation Clustered Heat Map. This function is intended
for use by NGCHM system developers.

## Usage

``` r
chmRegisterFunction(fn)
```

## Arguments

- fn:

  The function to be registered. This should be an object of class
  'ngchmJS'.

## Value

The registered function.

## See also

[`chmAddMenuItem()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddMenuItem-method.md)

[`chmNewFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewFunction.md)

[ngchmAxisFunction](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmAxisFunction-class.md)

[ngchmMatrixFunction](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmMatrixFunction-class.md)
