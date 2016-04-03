# NGCHMR
An R library for creating [Next-Generation Clustered Heat Maps (NG-CHM)](http://bioinformatics.mdanderson.org/main/NG-CHM:Overview
)

A docker image containing RStudio with this library preinstalled is available.  Follow the [instructions for using rstudio](https://github.com/rocker-org/rocker/wiki/Using-the-RStudio-image), but substitute "bmbroom/rstudio-ngchm" for the name of the repository.

To install this package into your own R system:
```
> install.packages("devtools")
> devtools::install_github("bmbroom/tsvio")
> devtools::install_github("bmbroom/NGCHMR", ref="stable")
```

You might also choose to install the beta version of the library, or for the really daring the master branch.

The installed library will be called NGCHM:
```
> library (NGCHM)
```

To use this library you need access to a local NG-CHM server.  A system administrator can create a local instance of a [docker](https://www.docker.com) based NG-CHM server by following [these instructions](http://bioinformatics.mdanderson.org/main/NG-CHM:Docker).

You will also need to create a [configuration file](http://bioinformatics.mdanderson.org/main/NG-CHM:NGCHMR-Config) describing, among other things, the NG-CHM server(s) to which you have access.

A basic guide to creating NG-CHMs using the library is available at http://bioinformatics.mdanderson.org/main/NG-CHM:NGCHMR-Using .
