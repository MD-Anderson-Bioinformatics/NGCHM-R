# Make NG-CHM Private on Server

This function makes a specific NG-CHM (Next-Generation Clustered Heat
Map) private on a specified server.

## Usage

``` r
chmMakePrivate(server, chm)

# S4 method for class 'ngchmServer,character'
chmMakePrivate(server, chm)

# S4 method for class 'ngchmServer,ngchm'
chmMakePrivate(server, chm)

# S4 method for class 'character,ngchm'
chmMakePrivate(server, chm)

# S4 method for class 'character,character'
chmMakePrivate(server, chm)
```

## Arguments

- server:

  An object of class 'ngchmServer' representing the server where the
  NG-CHM is hosted.

- chm:

  A single character string specifying the name of the NG-CHM to be made
  private.

## Value

No return value. The function is called for its side effect of making
the specified NG-CHM private on the specified server.

## See also

[ngchmServer](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmServer-class.md)

[ngchm](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchm-class.md)

[`chmInstall()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmInstall-method.md)

[`chmUninstall()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmUninstall-method.md)

[`chmMakePublic()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmMakePublic-method.md)
