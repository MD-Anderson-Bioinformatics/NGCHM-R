# Get the URL for an installed NGCHM.

Return the URL for accessing the specified Next Generation Clustered
Heat Map (NGCHM) on the specified server.

## Usage

``` r
chmGetURL(chm, ...)

# S4 method for class 'character'
chmGetURL(chm, server = NULL, ...)

# S4 method for class 'ngchm'
chmGetURL(chm, server = NULL, ...)
```

## Arguments

- chm:

  A single character string specifying the name of the NG-CHM.

- ...:

  Ignored.

- server:

  The server on which to view the NGCHM

## Value

A character string representing the URL of the specified NG-CHM on the
specified server.

## See also

[ngchmServer](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmServer-class.md)

[ngchm](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchm-class.md)
