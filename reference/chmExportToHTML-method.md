# Export a standalone HTML containing the NGCHM to a file.

Create a standalone HTML containing the NGCHM in the specified file.
This function requires **Java 11** and the
**[NGCHMSupportFiles](https://github.com/MD-Anderson-Bioinformatics/NGCHMSupportFiles)**
package.

## Usage

``` r
chmExportToHTML(
  chm,
  filename,
  overwrite = FALSE,
  shaidyMapGen,
  shaidyMapGenJava,
  shaidyMapGenArgs,
  ngchmWidgetPath
)
```

## Arguments

- chm:

  The NGCHM to generate the HTML for

- filename:

  The file in which to save the HTML

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

- ngchmWidgetPath:

  Path to location of ngchm Widget (ngchmWidget-min.js). Defaults to
  environment variable NGCHMWIDGETPATH.

## Value

filename

## Details

The NGCHMSupportFiles package can be installed from the R-universe
repository:  
  
`install.packages('NGCHMDemoData', `  
`repos = c('https://md-anderson-bioinformatics.r-universe.dev',`  
`'https://cloud.r-project.org'))`
