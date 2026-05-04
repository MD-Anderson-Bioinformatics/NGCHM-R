# Register a Javascript function for use in the NGCHM toolbox.

This function registers a Javascript function that can included in the
toolbox of an NGCHM. This function is intended for use by NGCHM system
developers.

## Usage

``` r
chmRegisterToolboxFunction(tbtype, menulabel, jsfn)
```

## Arguments

- tbtype:

  A single character string specifying the type of the toolbox function.

- menulabel:

  A single character string specifying the menu label of the toolbox
  function.

- jsfn:

  The function to be registered. This should be an object of class
  'ngchmJS'.

## Value

None. This function is used for its side effects of registering a new
toolbox function.

## See also

[`chmNewFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewFunction.md)

[ngchmAxisFunction](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmAxisFunction-class.md)

[ngchmMatrixFunction](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmMatrixFunction-class.md)
