# Add a CHM-specific axis type function to a NGCHM.

Adds a CHM-specific axis type function to a Next Generation Clustered
Heat Map (NGCHM) and returns the extended CHM. Multiple axis type
functions may be added to either axis. When the NGCHM is made, any
specific Axis functions matching the specified axis type will be
automatically added to the appropriate axis menu.

## Usage

``` r
chmAddSpecificAxisTypeFunction(chm, where, type, label, func)

# S4 method for class 'ngchm,character,character,character,ngchmJS'
chmAddSpecificAxisTypeFunction(chm, where, type, label, func)

# S4 method for class 'ngchm,character,character,character,character'
chmAddSpecificAxisTypeFunction(chm, where, type, label, func)
```

## Arguments

- chm:

  The chm to add the axis type to.

- where:

  The axis to add the axis type to. Must be either "row", "column", or
  "both".

- type:

  The type expected by the specified function.

- label:

  The label to use if and when the function is added to the menu.

- func:

  A javascript function that accepts a list of values of that type. If a
  string is provided, the function is obtained by calling
  chmGetFunction.

## Value

The extended chm.

## See also

[`chmListTypes()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmListTypes.md)

[ngchmAxisType](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmAxisType-class.md)
