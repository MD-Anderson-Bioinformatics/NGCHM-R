# Create a new Dataset for a NGCHM.

This function creates a new Dataset suitable for attaching to a Next
Generation Clustered Heat Map.

## Usage

``` r
chmNewDataset(
  name,
  description,
  data,
  row.type = NULL,
  column.type = NULL,
  row.covariates = NULL,
  column.covariates = NULL
)
```

## Arguments

- name:

  The filename prefix under which the dataset will be saved to the
  ngchm.

- description:

  A description of the dataset.

- data:

  A matrix containing the data in the dataset. Must have rownames and
  colnames.

- row.type:

  The type, if any, of the dataset rows.

- column.type:

  The type, if any, of the dataset columns.

- row.covariates:

  An optional list of row covariates.

- column.covariates:

  An optional list of column covariates.

## Value

An object of class ngchmDataset

## See also

[ngchmDataset](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmDataset-class.md)

[ngchmCovariate](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmCovariate-class.md)

[`chmAddDataset()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddDataset-method.md)
