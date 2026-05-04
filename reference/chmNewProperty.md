# Create a new Property for adding to a NGCHM.

This function creates a new Property object for adding to a Next
Generation Clustered Heat Map.

## Usage

``` r
chmNewProperty(label, value)
```

## Arguments

- label:

  The property label

- value:

  The property value

## Value

An object of class ngchmProperty

## See also

[ngchm](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchm-class.md)

[`chmAddProperty()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddProperty-method.md)

## Examples

``` r
prop <- chmNewProperty(
  "chm.info.caption",
  "This is a nifty new CHM."
)
```
