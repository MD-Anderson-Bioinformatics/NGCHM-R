# Add an NG-CHM to an NG-CHM collection.

Add the given Next-Generation Clustered Heat Map (NG-CHM) to the
specified collection (default: current collection).

## Usage

``` r
chmInstall(chm, ...)

# S4 method for class 'ngchm'
chmInstall(chm, path, ...)
```

## Arguments

- chm:

  The NGCHM to install.

- ...:

  Additional server (protocol) specific parameters.

- path:

  The path to the collection in which to install the NGCHM.

## Value

The installed chm.

## See also

[ngchmServer](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmServer-class.md)

[ngchm](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchm-class.md)

[`chmUninstall()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmUninstall-method.md)

[`chmMakePrivate()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmMakePrivate-method.md)

[`chmMakePublic()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmMakePublic-method.md)
