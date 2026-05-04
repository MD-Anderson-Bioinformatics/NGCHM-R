# Create an ngchmServer object from a specification.

Create an ngchmServer object called 'serverName' from the specification
'serverSpec' (see details). serverOptions override those in the
specification files option by option. The new ngchmServer object is
returned and registered so that it can be referenced by name, including
retrieval using chmServer.

## Usage

``` r
chmCreateServer(serverName, serverSpec = NULL, serverOptions = NULL)
```

## Arguments

- serverName:

  The name of the new server object.

- serverSpec:

  The specification for the server (defaults to servername).

- serverOptions:

  A named list of server options.

## Value

The created (and registered) ngchmServer object.

## Details

serverSpec can be any of:

- A configuration directory path. :

  The specification will be read from a file 'config.txt' in that
  directory.

- An NGCHM server URL (ending in '/chm' or '/Viewer' for instance). :

  A minimal specification will be inferred. Known methods for uploading
  NGCHMs to the server will be autoprobed unless specified manually.

- A URL referencing a configuration file (must end in '/config.txt'). :

  The specification will be read from the specified URL.

serverOptions can include both protocol-specific options and the
following generic options:

- 'serverURL'. :

  The URL for the NGCHM server.

- 'serverProtocol'. :

  The protocol to be used for uploading etc. NGCHMs to the server.

- 'jarFile'. :

  The jarFile used to build NGCHMs.

- 'traceLevel'. :

  The amount of trace to output. Defaults to "PROGRESS".

## See also

[`chmServer()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmServer.md)

[ngchmServer](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmServer-class.md)

[`ngchmGetServerProtocol()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmGetServerProtocol.md)

[ngchmServerProtocol](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmServerProtocol-class.md)
