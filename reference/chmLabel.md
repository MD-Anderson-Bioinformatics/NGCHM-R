# Get the label/name of an NG-CHM object.

Get the label/name of an NG-CHM object.

## Usage

``` r
chmLabel(x)
```

## Arguments

- x:

  The NG-CHM object to get the label/name of. Can be:

  - An object of class ngchm

  - An object of class ngchmLayer

  - An object of class ngchmDataset

  - An object of class ngchmBar

  - An object of class ngchmCovariate

  - An object of class ngchmColormap

## Value

A character string (or a vector of strings for an ngchmColormap)

## See also

[ngchm](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchm-class.md)

## Examples

``` r
chmLabel(chmNew("New CHM"))
#> [1] "New CHM"
```
