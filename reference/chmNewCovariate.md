# Create a new Covariate for adding to an NGCHM auxilary dataset.

This function creates a new Covariate suitable for a covariate bar or
attaching to an NGCHM auxilary dataset.

## Usage

``` r
chmNewCovariate(
  fullname,
  values,
  value.properties = NULL,
  type = NULL,
  covabbv = NULL
)
```

## Arguments

- fullname:

  The full (human readable) name of the covariate.

- values:

  A named vector of values (character, logical, or numeric).

- value.properties:

  An ngchmColormap mapping values to properties.

- type:

  The string "discrete" or the string "continuous". (Defaults to
  continuous for numeric values, to discrete otherwise.)

- covabbv:

  The short R-compatible identifier used to identify the covariate
  (derived from fullname if not specified).

## Value

An object of class ngchmCovariate.

## See also

[ngchmCovariate](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmCovariate-class.md)

[`chmAddCovariate()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddCovariate-method.md)

[`chmNewColorMap()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewColorMap.md)
