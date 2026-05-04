# Add a covariate bar to a NGCHM.

Add a covariate bar to a Next Generation Clustered Heat Map (NGCHM) and
return the extended CHM. If passed a covariate, a covariate bar will be
created (using any optional parameters supplied) and added.

## Usage

``` r
chmAddCovariateBar(chm, where, covar, ...)

# S4 method for class 'ngchm,character,ngchmBar'
chmAddCovariateBar(chm, where, covar)

# S4 method for class 'ngchm,character,ngchmCovariate'
chmAddCovariateBar(chm, where, covar, ...)

# S4 method for class 'ngchm,character,list'
chmAddCovariateBar(chm, where, covar, ...)
```

## Arguments

- chm:

  The chm to add the covariate bar to.

- where:

  The chm axis(axes) to add the covariate bar to. Must be one of "row",
  "column", or "both".

- covar:

  The covariate or covariate bar (or a list of them) to add to the chm.

- ...:

  Additional parameters passed to chmNewCovariateBar if covar is a
  covariate.

## Value

The extended chm.

## Details

If a covariate bar with the same name already exists on the specified
axis or axes, the existing bar will be replaced by the new bar.

## See also

[`chmNewCovariate()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewCovariate.md)

[`chmNewCovariateBar()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewCovariateBar.md)

[ngchmCovariate](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmCovariate-class.md)
