# Add a menu entry to a NGCHM.

Add a popup menu entry to a Next Generation Clustered Heat Map (NGCHM)
and return the extended CHM.

## Usage

``` r
chmAddMenuItem(chm, where, label, func)

# S4 method for class 'ngchm,character,character,ngchmJS'
chmAddMenuItem(chm, where, label, func)

# S4 method for class 'ngchm,character,character,character'
chmAddMenuItem(chm, where, label, func)
```

## Arguments

- chm:

  The chm to add the menu entry to.

- where:

  The chm menu(s) to add the menu entry to. Must be one of "row",
  "column", "both", or "element".

- label:

  The label to display in the menu entry.

- func:

  The javascript function to invoke when the menu entry is selected.

## Value

The extended chm.

## See also

[ngchmMenuItem](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmMenuItem-class.md)
