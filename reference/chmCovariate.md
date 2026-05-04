# Get a covariate attached to an NG-CHM dataset.

Get a covariate attached to an NG-CHM dataset.

## Usage

``` r
chmCovariate(dataset, fullname, where)
```

## Arguments

- dataset:

  The NG-CHM dataset to get the covariate from.

- fullname:

  The full name of the covariate to get. If no covariate with that name
  exists, return NULL.

- where:

  The axis or axes on which to look for the covariate Can be "row",
  "column", or "both" (default).

## Value

A ngchmCovariate or NULL.

## See also

[ngchmCovariate](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmCovariate-class.md)

chmNewCovariate

chmCovariateBar

## Examples

``` r
# If the NGCHMDemoData package is installed, use it to create demo usage
if (requireNamespace("NGCHMDemoData", quietly = TRUE)) {
  data(TCGA.GBM.Demo, package = "NGCHMDemoData")
  dataset <- chmNewDataset("gbmexpr", "TCGA GBM Expression Data", TCGA.GBM.ExpressionData)
  dataset <- chmAddCovariate(
    dataset, "column",
    chmNewCovariate("TP53 Mutation", TCGA.GBM.TP53MutationData)
  )
  tp53_mutation <- chmCovariate(dataset, "TP53 Mutation")
}
# Small example not requiring NGCHMDemoData
matrix <- matrix(rnorm(100),
  nrow = 10, ncol = 10,
  dimnames = list(paste0("r", 1:10), paste0("c", 1:10))
)
dataset <- chmNewDataset("Demo", "Random Demo Dataset", matrix)
covariate <- setNames(rnorm(10), colnames(matrix))
dataset <- chmAddCovariate(dataset, "column", chmNewCovariate("Random Covariate", covariate))
random_covariate <- chmCovariate(dataset, "Random Covariate")
```
