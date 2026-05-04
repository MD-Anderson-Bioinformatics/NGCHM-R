# List known axis types.

This function returns a list of the axis types for which axis- or
matrix- menu entries may be defined.

## Usage

``` r
chmListTypes(re = ".*")
```

## Arguments

- re:

  Only types with names matching re are returned (default ".\*")

## Value

a character vector of axis type names

## See also

[`chmAddAxisType()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddAxisType-method.md)
