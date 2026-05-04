# Create a new Javascript function for adding to a NGCHM menu.

This function creates a new Javascript function object for adding to a
Next Generation Clustered Heat Map menu.

## Usage

``` r
chmNewFunction(
  name,
  description,
  implementation,
  extraParams = NULL,
  requires = NULL,
  global = FALSE
)
```

## Arguments

- name:

  The name of the Javascript function

- description:

  A short description of the Javascript function

- implementation:

  A string containing the javascript code required to define the
  function. When called the function is passed a list of selected values
  (e.g. labels). Additional parameters can be declared before the values
  parameter and must be resolved through currying (binding) before the
  function is used in menus.

- extraParams:

  An optional list of extra parameters. (Default NULL.)

- requires:

  An optional vector of (custom) Javascript function names that this
  function requires.

- global:

  A logical: TRUE if should be defined globally, not within a
  customization section. (Default FALSE.)

## Value

An object of class ngchmJS

## See also

[ngchmJS](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmJS-class.md)

[`chmAddMenuItem()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddMenuItem-method.md)

[`chmBindFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmBindFunction-method.md)

[`chmRegisterFunction()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRegisterFunction.md)

## Examples

``` r
alertFn <- chmNewFunction("showAlert", "Display the parameter in an alert box",
  "function showAlert(label) { alert(label); }",
  global = TRUE
)
dbLookup <- chmNewFunction(
  "dbLookup", "Lookup the parameter in a database",
  "function showAlert(database, label) { alert(database[label]); }",
  c("database")
)
```
