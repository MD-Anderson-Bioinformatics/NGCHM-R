# Get Javascript function name for accessing a specific string field in each element of string vector.

This function returns the name of a Javascript function thats accepts a
string vector as its parameter, and for each string in the vector splits
the string into fields separated by fieldsep, and accesses field idx
(zero origin). The function returns a vector of these fields.

## Usage

``` r
chmFieldAccessFunction(fieldsep, idx)
```

## Arguments

- fieldsep:

  The separator to be used for splitting the input string. This should
  be a single character string.

- idx:

  The index (zero origin) of the field to be returned after splitting
  the input string. This should be a single integer.

## Value

The name of the newly created field access function.

## Details

The name of the function returned for a specific fieldsep and idx will
be constant within an R session, but may differ between R sessions (or
if this library is unloaded and reloaded).

## See also

[`chmGetFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmGetFunction.md)

[`chmStringopFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmStringopFunction.md)

## Examples

``` r
# Create a new field access function that splits the input string at ',' and
# returns the first field.
chmFieldAccessFunction(',', 1)
#> [1] "chmFA0"
# Create a new field access function that splits the input string at '-' and
# returns the second field.
chmFieldAccessFunction('-', 2)
#> [1] "chmFA1"
```
