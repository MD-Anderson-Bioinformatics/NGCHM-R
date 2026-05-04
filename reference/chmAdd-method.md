# Add a list of objects to a NGCHM.

Each additional parameter is added to the NGCHM according to its type.
Objects that require additional information (such as an axis) cannot be
added using this function. Objects that can be added are layers
(including numeric matrices), datasets, and colormaps.

## Usage

``` r
chmAdd(chm, ...)

# S4 method for class 'ngchm'
chmAdd(chm, ...)
```

## Arguments

- chm:

  The chm to add the object(s) to.

- ...:

  Zero or more objects to add to the NGCHM.

## Value

The extended chm.

## See also

"chmAddAxisType"

"chmAddColormap"

"chmAddDataset"

"chmAddLayer"

"chmAddMetaData"
