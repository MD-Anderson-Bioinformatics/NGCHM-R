# Generic method to get a dimensions matrix from obj.

The return value must be NULL or a numeric matrix, each column of which
is a (reduced) dimension. The rows of the returned matrix must be named.

## Usage

``` r
getDimensions(obj, ...)

# Default S3 method
getDimensions(obj, ...)

# S3 method for class 'prcomp'
getDimensions(obj, ...)

# S3 method for class 'umap'
getDimensions(obj, ...)

# S3 method for class 'Seurat'
getDimensions(obj, dimName, ...)
```

## Arguments

- obj:

  The object from which to obtain the dimension(s).

- ...:

  Additional class-specific parameters for specifying the desired
  dimension.

- dimName:

  The name of the dimension matrix to obtain.

## Value

A matrix with one dimension per column and one named row per observation
in obj.

## See also

[`chmAddReducedDim()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddReducedDim.md)
