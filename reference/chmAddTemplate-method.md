# Add a file template to the NGCHM.

Add a file template to the NGCHM.

## Usage

``` r
chmAddTemplate(chm, source.path, dest.path, substitutions)

# S4 method for class 'ngchm,charOrFunction,character,optList'
chmAddTemplate(chm, source.path, dest.path, substitutions)
```

## Arguments

- chm:

  The chm to add the file template to.

- source.path:

  A string giving the path to the template, or a function that returns
  the template content as a string.

- dest.path:

  A string giving the relative path where to store the template in the
  generated CHM.

- substitutions:

  A list (may be empty) of substitutions to make in the template.

## Value

The extended chm.
