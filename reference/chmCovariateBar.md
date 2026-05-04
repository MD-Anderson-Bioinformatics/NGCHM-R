# Get a covariate bar attached to an NG-CHM.

Get a covariate bar attached to an NG-CHM.

## Usage

``` r
chmCovariateBar(hm, fullname, where)
```

## Arguments

- hm:

  The NG-CHM to get the covariate bar from.

- fullname:

  The full name of the covariate bar to get. If no covariate bar with
  that name exists, return NULL.

- where:

  The axis or axes on which to look for the covariate bar Can be "row",
  "column", or "both" (default).

## Value

An ngchmBar or NULL.

## See also

[ngchmBar](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmBar-class.md)

chmNewCovariateBar

chmCovariate

## Examples

``` r
# Examples using `chmNew()` require git to be installed and available.
if (FALSE) { # \dontrun{
  # If the NGCHMDemoData package is installed, use it to demo usage
  if (requireNamespace("NGCHMDemoData", quietly = TRUE)) {
    # Create example NGCHM with covariate bar
    data(TCGA.GBM.Demo, package = "NGCHMDemoData")
    hm <- chmNew("gbmexpr", TCGA.GBM.ExpressionData[1:50, 1:50])
    hm <- chmAddCovariateBar(
      hm, "column",
      chmNewCovariate("TP53 Mutation", TCGA.GBM.TP53MutationData[1:50])
    )
    # Get covariate bar by name
    tp53_covariate_bar <- chmCovariateBar(hm, "TP53 Mutation")
  }
  # Small example not requiring NGCHMDemoData
  matrix <- matrix(rnorm(100),
    nrow = 10, ncol = 10,
    dimnames = list(paste0("r", 1:10), paste0("c", 1:10))
  )
  hm <- chmNew("Demo", matrix)
  covariate <- setNames(rnorm(10), colnames(matrix))
  hm <- chmAddCovariateBar(hm, "column", chmNewCovariate("my covariate", covariate))
  my_covariate_bar <- chmCovariateBar(hm, "my covariate")
} # }
```
