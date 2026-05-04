# Add a covariate to an auxiliary dataset.

Add a covariate to an auxiliary dataset and return the extended dataset.
Do not confuse this function with the one for adding a covariate bar to
an NGCHM. For that, please refer to the function chmAddCovariateBar.

## Usage

``` r
chmAddCovariate(dataset, where, covariate)

# S4 method for class 'ngchmDataset,character,ngchmCovariate'
chmAddCovariate(dataset, where, covariate)
```

## Arguments

- dataset:

  The dataset to add the covariate to.

- where:

  The dataset axis to add the covariate to. Must be one of "row",
  "column", or "both".

- covariate:

  The covariate to add to the dataset.

## Value

The extended dataset.

## See also

[`chmNewCovariate()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewCovariate.md)

[ngchmCovariate](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmCovariate-class.md)
