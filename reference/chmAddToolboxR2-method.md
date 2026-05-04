# Add Toolbox R2 to NG-CHM

This function adds a toolbox of type R2 to a NG-CHM (Next-Generation
Clustered Heat Map) object.

## Usage

``` r
chmAddToolboxR2(CHM, axistype, datasetname, idstr)

# S4 method for class 'ngchm,character,character,character'
chmAddToolboxR2(CHM, axistype, datasetname, idstr)
```

## Arguments

- CHM:

  An object of class 'ngchm'.

- axistype:

  A single character string specifying the type of the axis.

- datasetname:

  A single character string specifying the name of the dataset.

- idstr:

  string to append to toolbox menu labels (default ”)

## Value

An updated 'ngchm' object with the new toolbox of type R2 added.
