# Get Javascript function name for performing a specific string operation on each element of a string vector.

This function returns the name of a Javascript function thats accepts a
string vector as its parameter, and for each string in the vector
performs the operation stringop on the string. Stringop must be valid
Javascript code that can be appended to a string value. The function
returns a vector of the resulting strings.

## Usage

``` r
chmStringopFunction(stringop)
```

## Arguments

- stringop:

  A javascript code fragment that can be applied to a string to yield
  another string.

## Value

A character string specifying the name of the new function.

## Details

The name of the function returned for a specific stringop will be
constant within an R session, but may differ between R sessions (or if
this library is unloaded and reloaded).

## See also

[`chmGetFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmGetFunction.md)

[`chmFieldAccessFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmFieldAccessFunction.md)
