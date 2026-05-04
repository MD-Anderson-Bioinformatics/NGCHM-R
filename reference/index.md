# Function Reference

## Creating and Saving an NG-CHM

- [`chmNew()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNew.md)
  : Create a new NGCHM.

&nbsp;

- [`chmExportToHTML()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmExportToHTML-method.md)
  : Export a standalone HTML containing the NGCHM to a file.

&nbsp;

- [`chmExportToFile()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmExportToFile-method.md)
  : Export a standalone NGCHM to a file.

## Covariate Bars

- [`chmNewCovariate()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewCovariate.md)
  : Create a new Covariate for adding to an NGCHM auxilary dataset.

&nbsp;

- [`chmAddCovariateBar()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddCovariateBar-method.md)
  : Add a covariate bar to a NGCHM.

&nbsp;

- [`chmAddCovariate()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddCovariate-method.md)
  : Add a covariate to an auxiliary dataset.

&nbsp;

- [`chmNewCovariateBar()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewCovariateBar.md)
  : Create a new covariate Bar for a NGCHM

&nbsp;

- [`chmCovariate()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmCovariate.md)
  : Get a covariate attached to an NG-CHM dataset.

&nbsp;

- [`chmCovariateBar()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmCovariateBar.md)
  : Get a covariate bar attached to an NG-CHM.

## Colors and Color Maps

- [`chmNewColorMap()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewColorMap.md)
  : Create a new Color Map for use in constructing a NGCHM

&nbsp;

- [`chmAddColormap()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddColormap-method.md)
  : Add a colormap to a NGCHM.

&nbsp;

- [`` `chmColorMap<-`() ``](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmColorMap-set.md)
  : Set the color map of an NG-CHM object

&nbsp;

- [`chmColorMap()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmColorMap.md)
  : Get the color map of an NG-CHM object.

&nbsp;

- [`chmColors()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmColors.md)
  : Get the colors of an ngchmColormap, ngchmLayer, ngchmBar, or
  ngchmCovariate.

&nbsp;

- [`` `chmColors<-`() ``](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmColors-set.md)
  : Set the colors of an ngchmColormap, ngchmLayer, ngchmBar, or
  ngchmCovariate.

&nbsp;

- [`chmAdd()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAdd-method.md)
  : Add a list of objects to a NGCHM.

## Data Layers

- [`chmNewDataLayer()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewDataLayer.md)
  : Create a new Data Layer for a NGCHM.

&nbsp;

- [`chmAddLayer()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddLayer-method.md)
  : Add a Layer to a NGCHM.

&nbsp;

- [`chmLayer()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmLayer.md)
  : Get a specified Data Layer from an NG-CHM.

&nbsp;

- [`chmAdd()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAdd-method.md)
  : Add a list of objects to a NGCHM.

## Specifying Axis Type

For automatically adding linkouts

- [`chmAddAxisType()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddAxisType-method.md)
  : Add an axis type to a NGCHM.

## Add Scatter Plot Coordinates

- [`chmAddPCA()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddPCA.md)
  : Add PCA coordinates to an NG-CHM.

&nbsp;

- [`chmAddUMAP()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddUMAP.md)
  : Add UMAP coordinates to an NG-CHM.

&nbsp;

- [`chmAddUWOT()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddUWOT.md)
  : Add UWOT::UMAP coordinates to an NG-CHM.

&nbsp;

- [`chmAddTSNE()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddTSNE.md)
  : Add TSNE coordinates to an NG-CHM.

&nbsp;

- [`chmAddReducedDim()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddReducedDim.md)
  : Add reduced dimension coordinates to an NG-CHM.

## Row and Column Ordering and Dendrograms

- [`` `chmColOrder<-`() ``](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmColOrder-method.md)
  : Set the column order of data shown in a NGCHM.

&nbsp;

- [`` `chmRowOrder<-`() ``](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmRowOrder-method.md)
  : Set the row order of data shown in a NGCHM.

## Map Properties

- [`chmHasProperty()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmHasProperty-method.md)
  : Determine if the NG-CHM has the given property.

&nbsp;

- [`chmGetProperty()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmGetProperty-method.md)
  : Get Property from NG-CHM

&nbsp;

- [`chmNewProperty()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewProperty.md)
  : Create a new Property for adding to a NGCHM.

&nbsp;

- [`chmAddProperty()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddProperty-method.md)
  : Add custom property to a NGCHM.

&nbsp;

- [`chmProperty()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmProperty.md)
  : Get the value of an NG-CHM property.

&nbsp;

- [`` `chmProperty<-`() ``](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmProperty-set.md)
  : Set the value of an NG-CHM property.

&nbsp;

- [`chmAdd()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAdd-method.md)
  : Add a list of objects to a NGCHM.

&nbsp;

- [`chmSetDisplayLength()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmSetDisplayLength-method.md)
  : Set number of characters to display for row or column labels

## Adding Gaps

- [`chmTreeGaps()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmTreeGaps.md)
  : Creates new treeCuts object
