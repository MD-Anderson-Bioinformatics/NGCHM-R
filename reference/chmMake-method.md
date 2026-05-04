# Compile a NGCHM.

Deprecated. Users should no longer call this method directly.

## Usage

``` r
chmMake(chm, ...)

# S4 method for class 'ngchm'
chmMake(chm, ...)
```

## Arguments

- chm:

  The NGCHM to compile.

- ...:

  Additional chmMake options that depend on the format of the NGCHM. For
  details of the additional parameters of format x see ngchmMakeFormat.x
  (e.g. ngchmMakeFormat.original).

## Value

The chm

## Details

Compiles the specified Next Generation Clustered Heat Map (NGCHM) in
preparation for installation.

## See also

[ngchmServer](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmServer-class.md)

[ngchm](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchm-class.md)

[`chmNew()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNew.md)

[`chmInstall()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmInstall-method.md)

[`ngchmMakeFormat.original()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmMakeFormat.original.md)
