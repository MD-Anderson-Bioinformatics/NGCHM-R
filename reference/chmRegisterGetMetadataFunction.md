# Define and register a Javascript function for obtaining a specific metadata value.

This function defines and registers a Javascript function for obtaining
a specific metadata value and returning it as a javascript list. The
function is suitable for use as an axis type accessor function.

## Usage

``` r
chmRegisterGetMetadataFunction(functionName, metadataColumnName)
```

## Arguments

- functionName:

  A single character string specifying the name of the function to be
  registered.

- metadataColumnName:

  A single character string specifying the name of the metadata column
  to be retrieved by the function.

## Value

The registered function.

## See also

[`chmAddAxisType()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddAxisType-method.md)

[`chmGetFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmGetFunction.md)

[`chmListFunctions()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmListFunctions.md)
