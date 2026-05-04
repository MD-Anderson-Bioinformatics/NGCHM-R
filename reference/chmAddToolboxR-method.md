# Add standard toolbox to an NG-CHM axis

This function adds a toolbox to a NG-CHM (Next-Generation Clustered Heat
Map) axis.

## Usage

``` r
chmAddToolboxR(CHM, axis, axistype, datasetname, idstr)

# S4 method for class 'ngchm,character,character,character,character'
chmAddToolboxR(CHM, axis, axistype, datasetname, idstr)
```

## Arguments

- CHM:

  An object of class 'ngchm'.

- axis:

  A single character string specifying the axis where the toolbox will
  be added. Can be "row", "column", or "both".

- axistype:

  A single character string specifying the type of the axis.

- datasetname:

  A single character string specifying the name of the dataset.

- idstr:

  string to append to toolbox menu labels (default ”)

## Value

An updated 'ngchm' object with the new toolbox added.
