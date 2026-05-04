# Installation

## R Packages Installation

The NG-CHM R package can be installed from CRAN:

``` r

install.packages("NGCHM")
```

The required NGCHMSupportFiles can be installed from [our R-Universe
repository](https://md-anderson-bioinformatics.r-universe.dev/packages):

``` r

install.packages("NGCHMSupportFiles", repos = c("https://md-anderson-bioinformatics.r-universe.dev"))
```

The optional NGCHMDemoData (used in the vignette examples) can similarly
be installed:

``` r

install.packages("NGCHMDemoData", repos = c("https://md-anderson-bioinformatics.r-universe.dev"))
```

## System Requirements

- R \>= 3.5
- Java \>= 11
- git
  - Mac/Linux: typically installed by default
  - Windows: Cygwin or Windows Subsystem for Linux (Windows 10)
- C compiler
  - Mac: Xcode
  - Linux: gcc
  - Windows: Rtools or Rtools40 (Windows 10)
- [NGCHMSupportFiles](https://md-anderson-bioinformatics.r-universe.dev/NGCHMSupportFiles)
  R package
  - This package requires Java \>= 11
  - This package contains a compiled .jar file and minified JavaScript
    file
- *Optional:* The following are only required if using a remote NG-CHM
  server:
  - tar, ssh, scp *(most users will not need these)*

## Docker Image

A Docker image containing RStudio, the NGCHM-R package, and all needed
dependencies is available on Docker Hub:
[ngchm/rstudio-ngchm](https://hub.docker.com/r/ngchm/rstudio-ngchm).

Detailed usage information for the docker image is available in this the
[RStudio NGCHM
GitHub](https://github.com/MD-Anderson-Bioinformatics/rstudio-ngchm)
project, and the YouTube Video, [How to Create Next-Generation Clustered
Heat Maps in R Studio](https://www.youtube.com/watch?v=O42w5P3A1_8).
