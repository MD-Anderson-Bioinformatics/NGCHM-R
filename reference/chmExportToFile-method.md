# Export a standalone NGCHM to a file.

Create a standalone viewer for the NGCHM in the specified file. This
function requires **Java 11** and the
**[NGCHMSupportFiles](https://github.com/MD-Anderson-Bioinformatics/NGCHMSupportFiles)**
package.

## Usage

``` r
chmExportToFile(
  chm,
  filename,
  overwrite = FALSE,
  shaidyMapGen,
  shaidyMapGenJava,
  shaidyMapGenArgs
)
```

## Arguments

- chm:

  The NGCHM to export

- filename:

  The file in which to save the rendered NGCHM

- overwrite:

  Overwrite file iff true (default false)

- shaidyMapGen:

  Path to shaidyMapGen jar file (default to value of environment
  variable SHAIDYMAPGEN)

- shaidyMapGenJava:

  Path to java executable with which to run shaidyMapGen (default to
  value of environment variable SHAIDYMAPGENJAVA or java)

- shaidyMapGenArgs:

  Additional arguments to pass to java when running shaidyMapGen
  (default to value of environment variable SHAIDYMAPGENARGS)

## Value

the rendered NGCHM

## Details

The NGCHMSupportFiles package can be installed from the R-universe
repository:  
  
`install.packages('NGCHMDemoData', `  
`repos = c('https://md-anderson-bioinformatics.r-universe.dev',`  
`'https://cloud.r-project.org'))`
