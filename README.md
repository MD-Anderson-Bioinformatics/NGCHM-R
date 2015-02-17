# NGCHMR
An R library for creating [Next-Generation Clustered Heat Maps (NG-CHM)](http://bioinformatics.mdanderson.org/main/NG-CHM:Overview
)

To install this package into R:
```
> install.packages("devtools")
> devtools::install_github("bmbroom/NGCHMR")
```

The installed library will be called NGCHM:
```
> library (NGCHM)
```

To use this library you need access to a local NG-CHM server.  A system administrator can create a local instance of a [docker](https://www.docker.com) based NG-CHM server by following [these instructions](http://bioinformatics.mdanderson.org/main/NG-CHM:Docker).

You will also need to create a [configuration file](http://bioinformatics.mdanderson.org/main/NG-CHM:NGCHMR-Config) describing, among other things, the NG-CHM server(s) to which you have access.

A basic guide to creating NG-CHMs using the library is available at http://bioinformatics.mdanderson.org/main/NG-CHM:NGCHMR-Using .
