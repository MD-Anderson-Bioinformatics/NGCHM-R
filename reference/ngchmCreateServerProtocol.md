# Create and register an NGCHM server protocol implementation.

This function creates and registers a protocol implementation for
manipulating an NGCHM server.

## Usage

``` r
ngchmCreateServerProtocol(
  protocolName,
  chmFormat,
  requiredParams,
  optionalParams,
  paramValidator,
  findCollection,
  createCollection,
  installMethod,
  uninstallMethod,
  makePrivate,
  makePublic,
  setCredentials
)
```

## Arguments

- protocolName:

  A single character string specifying the name of the protocol.

- chmFormat:

  A single character string specifying the format of the heat map.
  Defaults to "original".

- requiredParams:

  A character vector specifying the required parameters for the
  protocol, if any.

- optionalParams:

  A character vector specifying the optional parameters for the
  protocol, if any.

- paramValidator:

  A function(list) for validating the parameters specified for a new
  server.

- findCollection:

  A function(server,collection,path) that finds a collection on the
  server.

- createCollection:

  A function(server,collection,name) that creates a collection on the
  server.

- installMethod:

  A function(server,chm) that installs a heat map on the server.

- uninstallMethod:

  A function(server,chmname) that uninstalls a heat map from the server.

- makePrivate:

  A function(server,chmname) that makes a heat map private on the
  server.

- makePublic:

  A function(server,chmname) that makes a heat map public on the server.

- setCredentials:

  A function(server,credentialstring) that sets the credentials for the
  server.

## Value

An object of class 'ngchmServerProtocol' representing the new server
protocol.
