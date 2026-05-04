# Creates new treeCuts object

This function was designed to facilitate setting rowGapLocations and
colGapLocations in the
[`chmNew()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNew.md)
function. See examples section.

## Usage

``` r
chmTreeGaps(numberOfCuts)
```

## Arguments

- numberOfCuts:

  Number of tree cuts

## Value

[treeCuts](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/treeCuts-class.md)
object with specified number of tree cuts

## Examples

``` r
mychm <- chmNew("test_chm", rowGapLocations = chmTreeGaps(5))
```
