# Set the value of an NG-CHM property.

Set the value of an NG-CHM property.

## Usage

``` r
chmProperty(x, label) <- value
```

## Arguments

- x:

  The NG-CHM object on which to set the property.

- label:

  The name of the property to set. If no property with that name exists,
  a new property with that name is appended.

- value:

  A non-empty vector of character, logical, or numeric values.

## Value

The modified NG-CHM object.

## See also

[ngchm](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchm-class.md)

## Examples

``` r
hm <- chmNew("Empty")
chmProperty(hm, "chm.info.caption") <- "Nothing to see here"
```
