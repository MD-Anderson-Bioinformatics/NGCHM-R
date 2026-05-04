# Get information about a type name.

This function gets any registered information about a type name used for
determining row and column linkouts. Registration of a typename is
(currently) not required in order to use it, so it's possible for valid
type name not to have any registered information.

## Usage

``` r
chmGetTypeInfo(typename)
```

## Arguments

- typename:

  The name of the type.

## Value

Object of class "ngchm.type.info" containing basic information about the
type.

## See also

[`chmListTypes()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmListTypes.md)

[`chmRegisterType()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRegisterType.md)
