# Add a colormap to a NGCHM.

Add a colormap to a Next Generation Clustered Heat Map (NGCHM) and
return the extended CHM. Duplicate colormaps will be silently dropped.

## Usage

``` r
chmAddColormap(chm, colormap)

# S4 method for class 'ngchm,ngchmColormap'
chmAddColormap(chm, colormap)
```

## Arguments

- chm:

  The chm to add the colormap to.

- colormap:

  The colormap to add to the chm.

## Value

The extended chm.

## Details

Note that it is not necessary to explicitly add colormaps included with
data layers or classification bars. These will be included
automatically. Explicitly using this function is only required in order
to add additional predefined, but unused colormaps to the NGCHM.

## See also

[`chmNewColorMap()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewColorMap.md)

[ngchmColormap](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmColormap-class.md)
