# Add an auxiliary dataset to a NGCHM.

Add an auxiliary dataset to a Next Generation Clustered Heat Map (NGCHM)
and return the extended CHM. The auxiliary dataset will be stored with
the NGCHM and be available in whole or in part from the same server, for
use, for example, in custom Javascript functions. Do not confuse this
function with the one for adding an active data layer to the heatmap
itself. For that, please refer to the function chmAddLayer.

## Usage

``` r
chmAddDataset(chm, dataset)

# S4 method for class 'ngchm,ngchmDataset'
chmAddDataset(chm, dataset)
```

## Arguments

- chm:

  The chm to add the dataset to.

- dataset:

  The dataset to add to the chm.

## Value

The extended chm.

## See also

[`chmNewDataset()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewDataset.md)

[ngchmDataset](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmDataset-class.md)
