# Get a registered ngchmServer object for use in making and installing NGCHMs

This function returns a ngchmServer object that can be used when making
and installing a Next Generation Clustered Heat Map.

## Usage

``` r
chmServer(name)
```

## Arguments

- name:

  The name of the ngchmServer desired.

## Value

An object of class ngchmServer if found, NULL otherwise. If multiple
servers of the same name have been defined (in different namespaces),
the most recently defined is returned.

## See also

[`chmInstall()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmInstall-method.md)

[`chmUninstall()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmUninstall-method.md)

[ngchmServer](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmServer-class.md)
