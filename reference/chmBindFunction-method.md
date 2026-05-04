# Bind values to an existing JS function.

Create a new JS function by binding values to extra parameters of an
existing JS function.

## Usage

``` r
chmBindFunction(name, fn, bindings)

# S4 method for class 'character,ngchmJS,list'
chmBindFunction(name, fn, bindings)

# S4 method for class 'character,character,list'
chmBindFunction(name, fn, bindings)
```

## Arguments

- name:

  A single character string specifying the name of the function.

- fn:

  An object of class 'ngchmJS' representing the function to be bound.

- bindings:

  A list containing at least one parameter binding. Each list element
  binds one parameter, starting from the first unbound parameter, and
  the name of each list element must match the name of the corresponding
  parameter.

## Value

A new 'ngchmJS' object representing the bound function.

## See also

[`chmNewFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewFunction.md)
