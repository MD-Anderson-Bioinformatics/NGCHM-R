# Add reduced dimension coordinates to an NG-CHM.

Add (reduced) dimension coordinates from an object obj as hidden
covariate bars to an axis of an NG-CHM. Depending on the object type,
dimName and dimAxis can be used to specify the name of the dimension of
interest in obj.

## Usage

``` r
chmAddReducedDim(hm, axis, obj, dimName, maxDim, basename, dimAxis)
```

## Arguments

- hm:

  The NGCHM to add the coordinates to.

- axis:

  The NGCHM axis ("row" or "column") to add the coordinates to.

- obj:

  An object containing the (reduced) dimension.

- dimName:

  The name of the (reduced) dimension to create covariate bars for.

- maxDim:

  The maximum number of coordinates to add (default all).

- basename:

  The prefix to use for the coordinate names (defaults to dimName).

- dimAxis:

  The axis on obj containing the named dimension, if applicable.

## Value

The NGCHM with added coordinates.

## Details

One hidden covariate bar is added for each coordinate obtained from
`obj`. If specified, maxDim limits the maximum number of covariate bars
added to the chm.

Coordinates have names 'BASENAME.coordinate.N', where BASENAME is
specified by the parameter basename (defaults to dimName if omitted) and
N ranges from 1 to the number of added covariate bars.

`obj` can be a numeric matrix, each column of which is a (reduced)
dimension. In this case, dimName and dimAxis are not used for obtaining
the reduced dimension. The number of rows of the matrix must equal the
size of the specified NGCHM axis and each row of the matrix must be
uniquely named using the names from that axis of the NG-CHM.

`obj` can also be an instance of class className if there exists an S3
method getDimensions.className. The method takes the object as its first
parameter and up to two optional parameters, dimName and dimAxis, that
can be used to specify the desired dimension. The method's return value
is a matrix similar to the one described in the preceding paragraph.
This package defines methods for classes `prcomp` and `umap`.

## See also

[`chmAddPCA()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddPCA.md)

[`chmAddTSNE()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddTSNE.md)

[`chmAddUMAP()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddUMAP.md)

[`chmAddUWOT()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddUWOT.md)

[`getDimensions()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/getDimensions-method.md)

## Examples

``` r
# Examples using `chmNew()` require git to be installed.
if (FALSE) { # \dontrun{
  if (requireNamespace("NGCHMDemoData", quietly = TRUE)) {
    data(TCGA.GBM.EXPR, package = "NGCHMDemoData")
    mat <- TCGA.GBM.EXPR[1:10, 1:10]
    prc <- prcomp(mat)
    hm <- chmNew("Demo reduced dimension coordinates", mat)
    hm <- chmAddReducedDim(hm, "column", prc, "PCA", 3, "PC")
    umc <- umap::umap(t(mat), n_neighbors = 8)
    hm <- chmAddReducedDim(hm, "column", umc, "UMAP")
  }
  # Small example not requiring NGCHMDemoData
  matrix <- matrix(rnorm(100),
    nrow = 10, ncol = 10,
    dimnames = list(paste0("r", 1:10), paste0("c", 1:10))
  )
  prc <- prcomp(matrix)
  hm <- chmNew("Demo reduced dimension coordinates", matrix)
  hm <- chmAddReducedDim(hm, "column", prc, "PCA", 3, "PC")
  umc <- umap::umap(t(matrix), n_neighbors = 8)
  hm <- chmAddReducedDim(hm, "column", umc, "UMAP")
} # }
```
