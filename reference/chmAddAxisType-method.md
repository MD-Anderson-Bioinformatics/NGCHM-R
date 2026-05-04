# Add an axis type to a NGCHM.

Adds an axis type to a Next Generation Clustered Heat Map (NGCHM) and
returns the extended CHM. Multiple axis types may be added to either
axis. When the NGCHM is made, any Axis functions matching the specified
axis type will be automatically added to the appropriate axis menu, and
any Matrix functions matching the types of the rows and columns will be
automatically added to the matrix menu.

## Usage

``` r
chmAddAxisType(chm, where, type, func)

# S4 method for class 'ngchm,character,character,ngchmJS'
chmAddAxisType(chm, where, type, func)

# S4 method for class 'ngchm,character,character,character'
chmAddAxisType(chm, where, type, func)

# S4 method for class 'ngchm,character,character,missing'
chmAddAxisType(chm, where, type, func)
```

## Arguments

- chm:

  The chm to add the axis type to.

- where:

  The axis to add the axis type to. Must be either "row" or "column".

- type:

  The type to add to the specified axis.

- func:

  A javascript function that gets values of that type from the current
  selection. If a string is provided, the function is obtained by
  calling chmGetFunction.

## Value

The extended chm.

## See also

[`chmListTypes()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmListTypes.md)

[`chmRegisterAxisFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRegisterAxisFunction.md)

[`chmRegisterMatrixFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRegisterMatrixFunction.md)

[`chmRegisterTypeMapper()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRegisterTypeMapper.md)

[ngchmAxisType](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmAxisType-class.md)
