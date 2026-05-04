# Get the value of an NG-CHM property.

Get the value of an NG-CHM property.

## Usage

``` r
chmProperty(hm, label)
```

## Arguments

- hm:

  The NG-CHM object to get the property value from.

- label:

  The name of the property to get. If no property with that name exists,
  return NULL.

         Well-known property labels used by the NG-CHM system include:

         * "chm.info.caption"  A paragraph describing the NG-CHM's contents (set by user).
        * "chm.info.built.time"  The date and time the NG-CHM was saved (set by system).

## Value

A property value or NULL.

## See also

[ngchm](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchm-class.md)

## Examples

``` r
hm <- chmNew("Empty")
chmProperty(hm, "chm.info.caption")
#> NULL
```
