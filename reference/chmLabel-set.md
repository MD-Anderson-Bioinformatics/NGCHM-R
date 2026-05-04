# Set the label/name of an NG-CHM object

Set the label/name of an NG-CHM object

## Usage

``` r
chmLabel(x) <- value
```

## Arguments

- x:

  The NG-CHM object on which to set the label/name.

- value:

  The new name (a single character string).

## Value

The modified NG-CHM object.

## See also

[chmLabel](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmLabel.md)

## Examples

``` r
hm <- chmNew("Old name")
chmLabel(hm) <- "A new name"
```
