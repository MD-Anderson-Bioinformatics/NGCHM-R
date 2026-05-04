# Create an ngchmServer object for a managed NG-CHM server

Create an ngchmServer object called 'serverName' (see details). The new
ngchmServer object is returned and registered so that it can be
referenced by name, including retrieval using chmServer. This library
will communicate with the NG-CHM using the private address. Returned
URLs for viewing NG-CHMs will use the public address.

## Usage

``` r
chmCreateManagedServer(
  serverName,
  privateAddr,
  publicAddr = NULL,
  chmPort = 80,
  managerPort = 18080,
  serviceName = "default",
  ...
)
```

## Arguments

- serverName:

  The name of the new server object.

- privateAddr:

  Private IP name/address of the server.

- publicAddr:

  Public IP name/address of the server.

- chmPort:

  Port on which the chm viewer is listening.

- managerPort:

  Port on which the chm manager is listening.

- serviceName:

  Name of the chmManager service

- ...:

  Additional options passed to chmCreateServer

## Value

The created (and registered) ngchmServer object.

## See also

[`chmServer()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmServer.md)

[`chmCreateServer()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmCreateServer.md)
