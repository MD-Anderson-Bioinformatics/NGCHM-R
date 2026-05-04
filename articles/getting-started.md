# Getting Started

This vignette demonstrates how to:

- [Format Data for NG-CHMs](#format-data-for-ng-chms)
- [Create an NG-CHM](#create-an-ng-chm)
- [Add a covariate bar](#add-a-covariate-bar)
- [Export to HTML, .ngchm, or PDF](#export-to-html--ngchm-or-pdf)
- [Embed into Rmarkdown](#embed-into-rmarkdown)

Complete code block examples are presented first in [R Code
Examples](#r-code-examples), followed by the sections detailing each
step. The fully interactive NG-CHM is in the last section, [Resulting
NG-CHM](#resulting-ng-chm).

Packages used in these examples can be installed with:

``` r

install.packages("NGCHMDemoData", repos = c("https://md-anderson-bioinformatics.r-universe.dev"))
install.packages("NGCHMSupportFiles", repos = c("https://md-anderson-bioinformatics.r-universe.dev"))
install.packages("NGCHM")
```

## R Code Examples

Below are two code examples. The first uses the
[NGCHMDemoData](https://md-anderson-bioinformatics.r-universe.dev/NGCHMDemoData)
to create an NG-CHM and export it to HTML and .ngchm files. The second
performs the same task with generic code containing placeholders for
user-specific data files, covariate column names, and other variables.

Click to expand/collapse **code example using NGCHMDemoData**

``` r

library(NGCHMDemoData)
library(NGCHMSupportFiles)
library(NGCHM)
matrix_data_file <- system.file("extdata", "TCGA.BRCA.Expression.csv", package = "NGCHMDemoData")
matrix_data <- as.matrix(read.csv(matrix_data_file, header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE))
covariate_data_file <- system.file("extdata", "TCGA.BRCA.TP53Mutation.csv", package = "NGCHMDemoData")
covariate_data <- read.csv(covariate_data_file, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE) # read.csv returns a data.frame
covariate_vector <- covariate_data[["MutationState"]] # create vector
names(covariate_vector) <- rownames(covariate_data) # set the names
hm <- chmNew("TCGA BRCA Expression", matrix_data)
covariateBar <- chmNewCovariate("TP53 Mutation", covariate_vector)
hm <- chmAddCovariateBar(hm, "column", covariateBar)
chmExportToHTML(hm, "tcga-brca.html", overwrite = TRUE)
chmExportToFile(hm, "tcga-brca.ngchm", overwrite = TRUE)
```

[Back to top](#)

Click to expand/collapse **generic code example** for .csv input files

``` r

library(NGCHMSupportFiles)
library(NGCHM)
# In the command below, replace "<path to your matrix data file>" with the path to your matrix data file.
matrix_data_file <- "<path to your matrix data file>"
matrix_data <- as.matrix(read.csv(matrix_data_file, header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE))
# In the command below, replace "<path to your covariate data file>" with the path to your covariate data file.
covariate_data_file <- "<path to your covariate data file>"
covariate_data <- read.csv(covariate_data_file, row.names = 1, stringsAsFactors = FALSE) # read.csv returns a data.frame
# In the command below, replace "<covariate column name>" with the name of the column in your covariate data file.
covariate_vector <- covariate_data[["<covariate column name>"]] # create vector
names(covariate_vector) <- rownames(covariate_data) # set the names
# In the command below, replace "<name of your NG-CHM>" with the desired name of your NG-CHM.
hm <- chmNew("<name of your NG-CHM>", matrix_data)
# In the command below, replace "<name of your covariate>" with the desired name of your covariate.
covariateBar <- chmNewCovariate("<name of your covariate>", covariate_vector)
hm <- chmAddCovariateBar(hm, "column", covariateBar)
# In the command below, replace "<name of your HTML file>" with the desired name of your HTML file.
chmExportToHTML(hm, "<name of your HTML file>", overwrite = TRUE)
# In the command below, replace "<name of your .ngchm file>" with the desired name of your .ngchm file.
# IMPORTANT: The filename must end with '.ngchm' in order to open in the NG-CHM Viewer
chmExportToFile(hm, "<name of your .ngchm file>", overwrite = TRUE)
```

[Back to top](#)

## Format Data for NG-CHMs

For use by the NGCHM package, data must be formatted such that:

- The heat map data is a matrix with rownames and colnames
- The covariate data is a named vector.
  - At least one name in the row/column covariate data vector must be in
    the row/column names of the heat map matrix.

This example uses the
[NGCHMDemoData](https://md-anderson-bioinformatics.r-universe.dev/NGCHMDemoData)
package, which can be installed from [our R-Universe
repository](https://md-anderson-bioinformatics.r-universe.dev/packages).

The NGCHMDemoData includes a matrix of gene expression data containing
3437 genes (rows) and 200 samples (columns) of breast cancer data from
The Cancer Genome Atlas (TCGA). This will be converted into a matrix for
constructing the NG-CHM.

|  | TCGA-AO-A0JJ-01A | TCGA-E9-A1R4-01A | TCGA-E9-A6HE-01A | TCGA-E2-A1L9-01A | ... |
|----|----|----|----|----|----|
| TSPAN6 | 11.838 | 9.483 | 11.157 | 11.536 | ... |
| CFH | 12.033 | 10.835 | 10.824 | 12.878 | ... |
| ENPP4 | 11.372 | 10.749 | 9.721 | 11.008 | ... |
| SEMA3F | 12.722 | 12.393 | 13.338 | 12.451 | ... |
| ... | ... | ... | ... | ... | ... |

TCGA.BRCA.Expression.csv snippet {.table .expression-data}

**IMPORTANT: In order to be used as the basis for an NG-CHM, a matrix
should have both rownames and colnames.**

The NGCHMDemoData also includes a vector of TP35 mutation status for the
TCGA samples in the matrix. This data will be used to construct a
covariate bar.

|                  | MutationState |
|------------------|---------------|
| TCGA-AO-A0JJ-01A | WT            |
| TCGA-E9-A1R4-01A | WT            |
| TCGA-E9-A6HE-01A | WT            |
| TCGA-E2-A1L9-01A | WT            |
| TCGA-E9-A245-01A | WT            |
| TCGA-AO-A0JG-01A | WT            |
| TCGA-C8-A1HE-01A | WT            |
| TCGA-C8-A12W-01A | MUT           |
| ...              | ...           |

TCGA.BRCA.TP53Mutation.csv snippet {.table .mutation-data}

**IMPORTANT: In order to be used as the basis for a row/column covariate
bar, the data should be formatted as a named vector with at least one
name in common with the row/column names of the matrix.**

The NGCHMDemoData package includes these datasets as .csv files. In the
code example, the matrix data is read from the TCGA.BRCA.Expression.csv
file (in the NGCHMDemoData package), and formatted as a matrix. The
mutation data is read from the TCGA.BRCA.TP53Mutation.csv file, and
formatted as a named vector.

``` r

library(NGCHMDemoData)
# Read in the NGCHMDemoData expression data, and format as a matrix
matrix_data_file <- system.file("extdata", "TCGA.BRCA.Expression.csv", package = "NGCHMDemoData")
matrix_data <- as.matrix(read.csv(matrix_data_file, header = TRUE, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE))
# Read in the NGCHMDemoData mutation data, and format as a named vector
covariate_data_file <- system.file("extdata", "TCGA.BRCA.TP53Mutation.csv", package = "NGCHMDemoData")
covariate_data <- read.csv(covariate_data_file, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE) # read.csv returns a data.frame
covariate_vector <- covariate_data[["MutationState"]] # create vector
names(covariate_vector) <- rownames(covariate_data) # set the names
```

[Back to top](#)

## Create an NG-CHM

The
[`chmNew()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNew.md)
function creates the NG-CHM object. The first argument is the name of
the NG-CHM, and the second argument is the matrix data.

``` r

library(NGCHM)
hm <- chmNew("TCGA BRCA Expression", matrix_data)
```

[Back to top](#)

## Add a Covariate Bar

The function
[`chmNewCovariate()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmNewCovariate.md)
creates a covariate object. The first argument is the name of the
covariate, and the second argument is the named vector of covariate
data. The
[`chmAddCovariateBar()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddCovariateBar-method.md)
function adds the covariate to the NG-CHM. The first argument is the
NG-CHM, the second argument is the desired map axis for the covariate
bar (‘column’ or ‘row’), and the third argument is the covariate object.

``` r

covariateBar <- chmNewCovariate("TP53 Mutation", covariate_vector)
hm <- chmAddCovariateBar(hm, "column", covariateBar)
```

[Back to top](#)

## Export to HTML, .ngchm, or PDF

The NG-CHM can be exported to two different file types:

1.  **HTML File**: can be emailed to collaborators, opened in a web
    browser, or embedded in an RMarkdown file.
2.  **.ngchm File**: can be opened in the [NGCHM
    Viewer](https://www.ngchm.net/Downloads/ngChmApp.html).

Both methods use files from the
[NGCHMSupportFiles](https://md-anderson-bioinformatics.r-universe.dev/NGCHMSupportFiles)
package. When loaded, NGCHMSupportFiles sets environment variables
pointing to these additional files.

``` r

library(NGCHMSupportFiles)
```

### 1. HTML File

The function
[`chmExportToHTML()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmExportToHTML-method.md)
exports the NG-CHM to an HTML file. The first argument is the NG-CHM
created above. The second argument is the desired filename. The optional
argument `overwrite = TRUE` overwrites the file if it already exists.

``` r

chmExportToHTML(hm, "tcga-brca.html", overwrite = TRUE)
```

The file ‘tcga-brca.html’ can be shared with collaborators and opened in
a web browser.

### 2. .ngchm File

Alternatively,
[`chmExportToFile()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmExportToFile-method.md)
can be used to create a .ngchm file.

``` r

chmExportToFile(hm, "tcga-brca.ngchm", overwrite = TRUE)
```

The file ‘tcga-brca.ngchm’ can be opened in the NG-CHM Viewer.
**IMPORTANT: The filename must end with ‘.ngchm’ in order to open in the
NG-CHM Viewer**.

### 3. PDF File

To create a publication-quality PDF, there are several options:

![](data:image/svg+xml;base64,PHN2ZyBzdHlsZT0iZGlzcGxheTpub25lOyI+PHN5bWJvbCBpZD0iaWNvbi1tZW51LWJ1dHRvbi1maWxsIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGZpbGw9ImN1cnJlbnRDb2xvciIgY2xhc3M9ImJpIGJpLW1lbnUtYnV0dG9uLWZpbGwiIHZpZXdib3g9IjAgMCAxNiAxNiI+PHBhdGggZD0iTTEuNSAwQTEuNSAxLjUgMCAwIDAgMCAxLjV2MkExLjUgMS41IDAgMCAwIDEuNSA1aDhBMS41IDEuNSAwIDAgMCAxMSAzLjV2LTJBMS41IDEuNSAwIDAgMCA5LjUgMGgtOHptNS45MjcgMi40MjdBLjI1LjI1IDAgMCAxIDcuNjA0IDJoLjc5MmEuMjUuMjUgMCAwIDEgLjE3Ny40MjdsLS4zOTYuMzk2YS4yNS4yNSAwIDAgMS0uMzU0IDBsLS4zOTYtLjM5NnpNMCA4YTIgMiAwIDAgMSAyLTJoMTJhMiAyIDAgMCAxIDIgMnY1YTIgMiAwIDAgMS0yIDJIMmEyIDIgMCAwIDEtMi0yVjh6bTEgM3YyYTEgMSAwIDAgMCAxIDFoMTJhMSAxIDAgMCAwIDEtMXYtMkgxem0xNC0xVjhhMSAxIDAgMCAwLTEtMUgyYTEgMSAwIDAgMC0xIDF2MmgxNHpNMiA4LjVhLjUuNSAwIDAgMSAuNS0uNWg5YS41LjUgMCAwIDEgMCAxaC05YS41LjUgMCAwIDEtLjUtLjV6bTAgNGEuNS41IDAgMCAxIC41LS41aDZhLjUuNSAwIDAgMSAwIDFoLTZhLjUuNSAwIDAgMS0uNS0uNXoiIC8+PC9zeW1ib2w+PC9zdmc+)

1.  Create an HTML file [as described above](#html-file). Open the file
    in a web browser and create the view you want to save as a PDF
    (e.g. the detail map zoomed to the desired location, any desired
    rows/columns selected, covariate bars displayed, etc.). Then click
    the
    ![](data:image/svg+xml;base64,PHN2ZyBzdHlsZT0id2lkdGg6MWVtO2hlaWdodDoxZW07dmVydGljYWwtYWxpZ246bWlkZGxlOyIgdmlld2JveD0iMCAwIDE2IDE2Ij48dXNlIHhsaW5rOmhyZWY9IiNpY29uLW1lbnUtYnV0dG9uLWZpbGwiIC8+PC9zdmc+)
    button (top right corner) and select “Save Heat Map as PDF”.

2.  Create a .ngchm file [as described above](#ngchm-file). Open the
    .ngchm file in the [NG-CHM
    Viewer](https://tcga.ngchm.net/NGCHM/chm.html) and create the view
    you want to save as a PDF (e.g. the detail map zoomed to the desired
    location, any desired rows/columns selected, covariate bars
    displayed, etc.). Then click the
    ![](data:image/svg+xml;base64,PHN2ZyBzdHlsZT0id2lkdGg6MWVtO2hlaWdodDoxZW07dmVydGljYWwtYWxpZ246bWlkZGxlOyIgdmlld2JveD0iMCAwIDE2IDE2Ij48dXNlIHhsaW5rOmhyZWY9IiNpY29uLW1lbnUtYnV0dG9uLWZpbGwiIC8+PC9zdmc+)
    button (top right corner) and select “Save Heat Map as PDF”.

[Back to top](#)

## Embed into RMarkdown

The HTML file can be embedded into RMarkdown as an iframe using
[`htmltools::tags`](https://rstudio.github.io/htmltools/reference/builder.html).

``` r

library("htmltools")
htmltools::tags$iframe(src = "tcga-brca.html", width = "100%", height = 700)
```

[Back to top](#)

## Resulting NG-CHM

Below is the fully interactive NG-CHM created in this vignette.

[Bact to top](#)
