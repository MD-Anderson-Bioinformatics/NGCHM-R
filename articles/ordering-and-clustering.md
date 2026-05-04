# Ordering and Clustering

This vignette describes options for ordering rows and columns of an
NG-CHM, and the additional options available when the ordering is
hierarchical clustering. The YouTube video [How to Use Any Label Order
or Clustering Method with
NG-CHMs](https://www.youtube.com/watch?v=847breSor1I&list=PLIBaINv-Qmd0v2qD4AMbGf85Xd-T_yzEh),
also covers these topics.

These examples build on the setup from [Getting
Started](https://md-anderson-bioinformatics.github.io/NGCHM-R/articles/getting-started.md).

## Specifying Row / Column Order

The function
[`chmNew()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNew.md)
has optional arguments “rowOrder” and “colOrder” to specify the order of
the rows and columns, respectively. Values for these arguments can be a
vector, dendrogram, or function specifying the order of the
rows/columns.

For example, the following code block creates an NG-CHM with rows sorted
alphabetically and columns ordered randomly:

``` r

rowOrder <- sort(rownames(matrix_data))
colOrder <- colnames(matrix_data)[sample.int(length(colnames(matrix_data)))]
hm <- chmNew("tcga-brca", matrix_data, rowOrder = rowOrder, colOrder = colOrder)
```

## Hierarchical Clustering Options

For hierarchical clustering, additional options are available to specify
the distance measure and clustering method.

The arguments to
[`chmNew()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNew.md)
‘rowDist’ and ‘colDist’ specify the distance measure for rows and
columns, respectively. The available choices include those of the
[`stats::dist()`](https://rdrr.io/r/stats/dist.html) function, as well
as two additional distance metric options: ‘cosine’ and ‘correlation’.
The default choice is ‘correlation’, which computes the distance measure
as 1 minus the Pearson correlation among the rows/columns.

Arguments ‘rowAgglom’ and ‘colAgglom’ specify the clustering method, and
the available options are those of the
[`stats::hclust`](https://rdrr.io/r/stats/hclust.html) function. The
default is ‘ward.D2’.

This example specifies an NG-CHM with hierarchical clustering using the
Euclidean distance metric and complete linkage clustering algorithm for
both rows and columns:

``` r

hm <- chmNew("tcga-brca", matrix_data, rowDist = "euclidean", colDist = "euclidean", rowAgglom = "complete", colAgglom = "complete")
```

[Back to top](#)

## Explicit Clustering

Clustering may be performed explicitly (e.g. via `stats::hclust())` and
used in constructing an NG-CHM. The following creates an object of class
hclust from the distance matrix of the demo expression data, for both
the rows and columns, and uses those clustering results to construct an
NG-CHM.

``` r

rowClust <- stats::hclust(dist(matrix_data))
colClust <- stats::hclust(dist(t(matrix_data)))
hm <- chmNew("tcga-brca", matrix_data, rowOrder = rowClust, colOrder = colClust)
```

Similarly, an hclust object can be transformed into a dendrogram and
used for the row and/or column ordering. The example below uses the
clustering results from above as dendrograms:

``` r

rowDendrogram <- as.dendrogram(stats::hclust(dist(matrix_data)))
colDendrogram <- as.dendrogram(stats::hclust(dist(t(matrix_data))))
hm <- chmNew("tcga-brca", matrix_data, rowOrder = rowDendrogram, colOrder = colDendrogram)
```

[Back to top](#)
