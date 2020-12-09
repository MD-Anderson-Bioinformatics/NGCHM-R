# NGCHMR
An R package for creating [Next-Generation Clustered Heat Maps (NG-CHM)](https://bioinformatics.mdanderson.org/main/NG-CHM:Overview).

## Testing/Using a Docker image

An easy way to test the NG-CHM R package is use one of our docker images with the package preinstalled.  The docker containers are based on the [Rocker RStudio image](https://github.com/rocker-org/rocker).  Our Docker images are:

* ngchm/rstudio-ngchm, which contains RStudio plus the NGCHM-R package,
* ngchm/rstudio-ngchm-bioc, which also contains several Bioconductor packages,
* ngchm/rstudio-ngchm-sc, which also contains several commonly used R packages for single-cell analysis.

Our YouTube channel contains a [video playlist: NG-CHMs in R/R-Studio](https://link.ngchm.net/hpqAYMgpU), which includes several introductory videos that describe how to use the NG-CHM R package.

For more detail, refer to the [instructions for using the Rocker RStudio image](https://github.com/rocker-org/rocker/wiki/Using-the-RStudio-image), but substitute one of the docker images listed above for the name of the image to use.

## Installing on Linux or Mac
To install this package into your own R system:
```
> install.packages("devtools")
> devtools::install_github("MD-Anderson-Bioinformatics/tsvio")
> devtools::install_github("MD-Anderson-Bioinformatics/NGCHM-R", ref="stable")
```

You might also choose to install the beta version of the package, or for the really daring the master branch.

## Installing on Windows
Precompiled packages suitable for the Microsoft Windows version of R are available from our
[downloads site](https://www.ngchm.net/Downloads).  Install the tsvio package before the NGCHM package.

## Using the Package

The installed package will be called NGCHM:
```
> library (NGCHM)
```

A basic guide to creating NG-CHMs using the R package is available at https://bioinformatics.mdanderson.org/public-software/ngchm/#ng-chm-viewer-overview .

## Installing the Java Component (all platforms)

To use this package you need access to a Java application for generating standalone NG-CHMs and/or an NG-CHM server to which you can upload NG-CHMs.

To create standalone NG-CHMs you also need the Java application ShaidyMapGen.jar.  The easiest way to get a copy is to download it from
[http://tcga.ngchm.net/NGCHM/ShaidyMapGen.jar](https://tcga.ngchm.net/NGCHM/ShaidyMapGen.jar).
You can also download it from our [downloads site](https://www.ngchm.net/Downloads).
Finally, you can build it from the [source code](https://github.com/MD-Anderson-Bioinformatics/NG-CHM).

Once downloaded, set the environment variable SHAIDYMAPGEN to its location.

## Running an NG-CHM server

A system administrator can create a local instance of a [docker](https://www.docker.com) based NG-CHM server.  Instructions for running our new server container will be available shortly.  Contact us if you're waiting on them for a pre-release version.
