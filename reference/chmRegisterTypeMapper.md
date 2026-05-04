# Register a predefined Javascript function for converting values from one type to another.

This function registers a Javascript function that will be automatically
added to a Next Generation Clustered Heat Map as required for converting
values from one type into another more basic type. This function is
intended for use by NGCHM system developers.

## Usage

``` r
chmRegisterTypeMapper(fromtype, totype, op, ...)
```

## Arguments

- fromtype:

  The type of values the function expects as input.

- totype:

  The type of values the function will produce. The length of totype
  must be shorter than fromtype.

- op:

  The operation code for performing the conversion

- ...:

  Additional parameters required for specifying the conversion (op
  specific)

## Value

NULL

op can have the following values:

- 'field' Split source into fields separated by 'separator' and select
  field 'num' (0 origin)

- 'expr' Compute string expression 'expr'. 'return' value is a vector or
  a scalar (default)

- 'javascript' Evaluate javascript function 'fn' (deprecated)

## See also

[`chmAddAxisType()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddAxisType-method.md)

[`chmRegisterAxisFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRegisterAxisFunction.md)

[`chmRegisterMatrixFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRegisterMatrixFunction.md)

[`chmNewFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewFunction.md)
