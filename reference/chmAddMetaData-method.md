# Add MetaData to NG-CHM

This function adds metadata to a NG-CHM (Next-Generation Clustered Heat
Map) object.

## Usage

``` r
chmAddMetaData(chm, where, type, value)

# S4 method for class 'ngchm,character,character,character'
chmAddMetaData(chm, where, type, value)
```

## Arguments

- chm:

  An object of class 'ngchm'.

- where:

  A single character string specifying where to add the metadata. Can be
  "row", "column", or "both".

- type:

  A single character string specifying the type of the metadata.

- value:

  A character vector specifying the values of the metadata. If value is
  a character vector, elements of the vector will be attached as meta
  data to to NGCHM row of the same name.

## Value

An updated 'ngchm' object with the new metadata added.
