# Set the row order of data shown in a NGCHM.

This function sets the row order for a NG-CHM (Next-Generation Clustered
Heat Map) object.

## Usage

``` r
chmRowOrder(chm) <- value

# S4 method for class 'ngchm,optDendrogram'
chmRowOrder(chm) <- value
```

## Arguments

- chm:

  An object of class 'ngchm'.

- value:

  An object of class 'optDendrogram' or 'file' specifying the new row
  order. If value is NULL, the labels will be displayed in the same
  order they are found in the first data layer. If value is a character
  vector, the labels will be displayed in that order. If value is a
  dendrogram, the labels displayed in the order they occur in a depth
  first traversal of the tree.

## Value

An updated 'ngchm' object with the new row order.

## See also

"chmColOrder\<-"
