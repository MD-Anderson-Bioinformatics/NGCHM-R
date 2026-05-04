# Adding Linkouts

Linkouts allow for a deeper exploration of the NG-CHM data by providing
direct links to NCBI Databases, GeneCards, etc. This vignette
demonstrates how to add linkouts to an NG-CHM.

These examples build on the setup from [Getting
Started](https://md-anderson-bioinformatics.github.io/NGCHM-R/articles/getting-started.md).

## Set Axis Type for Linkouts

To add linkouts, specify the axis type with
[`chmAddAxisType()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmAddAxisType-method.md).
For the demo data in this example, row labels correspond to genes, and
are specified as HUGO gene symbols. The column labels are full
28-character TCGA aliquot barcodes.

``` r

hm <- chmNew("tcga-brca", matrix_data)
hm <- chmAddAxisType(hm, "row", "bio.gene.hugo")
hm <- chmAddAxisType(hm, "column", "bio.tcga.barcode.sample.vial.portion.analyte.aliquot")
```

## Available Axis Types

The available axis types are:

- **bio.gene.entrezid**: NCBI Entrez Gene Identifier
- **bio.gene.hugo**: HUGO gene symbol
- **bio.geo.acc**: GEO accession id
- **bio.go**: Gene Ontology accession identifier
- **bio.mdacc.pathwayid**:
  [PathwaysWeb](https://bioinformatics.mdanderson.org/public-software/pathwaysweb/)
  pathway identifier
- **bio.mirna**: miRNA identifier
- **bio.pathway.msigdb.name**: msig DB pathway name
- **bio.protein.uniprotid**: Uniprot protein identifier
- **bio.pubmed**: PubMed identifier
- **bio.tcga.barcode.sample**: First 15 characters of TCGA aliquot
  barcode
- **bio.tcga.barcode.sample.vial**: First 16 characters of TCGA aliquot
  barcode
- **bio.tcga.barcode.sample.vial.portion**: First 19 characters of TCGA
  aliquot barcode
- **bio.tcga.barcode.sample.vial.portion.analyte**: First 20 characters
  of a TCGA aliquot barcode
- **bio.tcga.barcode.sample.vial.portion.analyte.aliquot**: Full 28
  character TCGA aliquot barcode
- **bio.transcript.ensembl**: Ensembl transcript identifier
- **bio.transcriptid**: Entrez transcription identifier
- **scholar**: Google Scholar search term
- **search**: Generic search term

[Back to top](#)

## Resulting NG-CHM

Below is the resulting NG-CHM. To access the linkouts, click on the row
or column labels to select them, then right click on the selection. This
will open the label menu, displaying the linkout options.

[Back to top](#)
