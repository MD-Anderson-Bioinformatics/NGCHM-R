# Create NG-CHM Properties

This function creates one or more NG-CHM (Next-Generation Clustered Heat
Map) properties.

## Usage

``` r
chmProperties(...)
```

## Arguments

- ...:

  Named arguments representing the properties to be created. Each
  argument should be a single value of type character, double, integer,
  or logical.

## Value

A list of properties. Each property is represented as a list with two
elements: 'label' and 'value'.

## See also

[`chmAdd()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAdd-method.md)

## Examples

``` r
# Create three properties: 'prop1', 'prop2', and 'prop3'.
props <- chmProperties(prop1 = "value1", prop2 = 2, prop3 = TRUE)
```
