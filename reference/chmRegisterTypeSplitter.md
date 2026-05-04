# Define and register a Javascript function for converting a lists of type values into single values.

This function defines and registers a Javascript function for converting
a list of type values separated by the specified separator into the
single values, and registers it as a type mapper.

## Usage

``` r
chmRegisterTypeSplitter(functionName, listtype, itemtype, separator)
```

## Arguments

- functionName:

  A single character string specifying the name of the function to be
  registered.

- listtype:

  A single character string specifying the type of the list to be split.

- itemtype:

  A single character string specifying the type of the items in the list
  after splitting.

- separator:

  A single character string specifying the separator to be used for
  splitting.

## Value

None. This function is used for its side effects of registering a new
type splitter.

## See also

[`chmGetFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmGetFunction.md)

[`chmListFunctions()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmListFunctions.md)

[`chmRegisterTypeMapper()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRegisterTypeMapper.md)
