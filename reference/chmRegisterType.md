# Register a type name.

This function registers a type name used for determining row and column
linkouts. This function is intended to be used by NGCHM system
developers to record basic information about the semantic interpretation
of a type name. Registration of a typename is (currently) not required
in order to use it.

## Usage

``` r
chmRegisterType(typename, description)
```

## Arguments

- typename:

  A character vector specifying the name(s) of the type(s) to be
  registered.

- description:

  A single character string specifying the description of the type(s).

## Value

None. This function is used for its side effects of registering a new
type.

## See also

[`chmListTypes()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmListTypes.md)

[`chmGetTypeInfo()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmGetTypeInfo.md)

[`chmRegisterTypeMapper()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRegisterTypeMapper.md)
