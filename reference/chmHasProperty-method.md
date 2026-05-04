# Determine if the NG-CHM has the given property.

This function checks if a specific property exists in a NG-CHM
(Next-Generation Clustered Heat Map) object.

## Usage

``` r
chmHasProperty(object, label)

# S4 method for class 'ngchmVersion2,character'
chmHasProperty(object, label)
```

## Arguments

- object:

  An object of class 'ngchmVersion2' representing the NG-CHM to be
  checked.

- label:

  A single character string or a vector of character strings specifying
  the label(s) of the property(ies) to be checked.

## Value

A logical value indicating whether the specified property(ies) exist in
the 'ngchmVersion2' object. If 'label' is a vector, a logical vector is
returned.
