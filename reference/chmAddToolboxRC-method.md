# Add Toolbox RC to NG-CHM

This function adds a toolbox of type RC to a NG-CHM (Next-Generation
Clustered Heat Map) object.

## Usage

``` r
chmAddToolboxRC(CHM, rowtype, coltype, datasetname, idstr)

# S4 method for class 'ngchm,character,character,character,character'
chmAddToolboxRC(CHM, rowtype, coltype, datasetname, idstr)
```

## Arguments

- CHM:

  An object of class 'ngchm'.

- rowtype:

  A single character string specifying the type of the row.

- coltype:

  A single character string specifying the type of the column.

- datasetname:

  A single character string specifying the name of the dataset.

- idstr:

  string to append to toolbox menu labels (default ”)

## Value

An updated 'ngchm' object with the new toolbox of type RC added.
