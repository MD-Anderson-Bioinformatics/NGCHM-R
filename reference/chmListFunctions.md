# List the predefined Javascript functions available for use in NGCHM menus.

This function lists the predefined Javascript functions available for
use in NGCHM menus.

## Usage

``` r
chmListFunctions(re = ".*")
```

## Arguments

- re:

  The regular expression to match. This should be a single character
  string. Default is ".\*", which matches all functions.

## Value

A string containing the names and descriptions of the matching
functions.

## See also

[`chmAddMenuItem()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddMenuItem-method.md)

[`chmGetFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmGetFunction.md)

[`chmRegisterFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRegisterFunction.md)

[`grep()`](https://rdrr.io/r/base/grep.html)

## Examples

``` r
chmListFunctions() # List all functions.
#> [1] "\tSimple reference\ngetLabelValue\tThis returns the label at the specified index as a list of values.  Can be used whenever the label itself is of the correct type.\nchmFA0\tSplits each input string at ,, and returns field 1.\nchmFA1\tSplits each input string at -, and returns field 2."
chmListFunctions('^chm') # List all functions whose names start with 'chm'.
#> [1] "chmFA0\tSplits each input string at ,, and returns field 1.\nchmFA1\tSplits each input string at -, and returns field 2."
```
