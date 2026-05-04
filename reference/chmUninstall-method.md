# Remove an NG-CHM from Server

This function removes a specific NG-CHM (Next-Generation Clustered Heat
Map) from a specified server.

## Usage

``` r
chmUninstall(chm, ...)

# S4 method for class 'character'
chmUninstall(chm, server = NULL, ...)

# S4 method for class 'ngchm'
chmUninstall(chm, ...)
```

## Arguments

- chm:

  A single character string specifying the NG-CHM's name, or an object
  of class "ngchm" representing the NG-CHM to be uninstalled.

- ...:

  Additional server (protocol) specific parameters.

- server:

  An object of class 'ngchmServer' or a character string representing
  the server from which the NG-CHM is to be uninstalled. If not
  provided, the current server is used.

## Value

No return value. The function is called for its side effect of
uninstalling the specified NG-CHM from the specified server.

## See also

[ngchmServer](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmServer-class.md)

[ngchm](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchm-class.md)

[`chmInstall()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmInstall-method.md)
