# Get Property from NG-CHM

This function retrieves a specific property from a NG-CHM
(Next-Generation Clustered Heat Map) object.

## Usage

``` r
chmGetProperty(object, label)

# S4 method for class 'ngchmVersion2,character'
chmGetProperty(object, label)
```

## Arguments

- object:

  An object of class 'ngchmVersion2' representing the NG-CHM from which
  the property is to be retrieved.

- label:

  A single character string specifying the label of the property to be
  retrieved.

## Value

The property associated with the specified label in the 'ngchmVersion2'
object.
