# Create a new object representing a NGCHM server.

This function creates a new object that represents a NGCHM server.

## Usage

``` r
chmNewServer(
  serverName,
  serverPort = 8080,
  deployServer = NULL,
  protoOpts = NULL,
  jarFile = NULL,
  serverURL = NULL
)
```

## Arguments

- serverName:

  The DNS name of the NGCHM server.

- serverPort:

  The port on which the server is listening.

- deployServer:

  The DNS name to use when deploying a NGCHM (defaults to serverName).

- protoOpts:

  A list of protocol-specific parameters

- jarFile:

  The location of the heatmap build jar file to use when making a NGCHM
  (defaults to jar file on serverURL WS).

- serverURL:

  The URL used to access the NGCHM server (defaults to
  serverName:serverPort/chm).

## Value

An object of class ngchmServer

## See also

[ngchmServer](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmServer-class.md)

[`chmInstall()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmInstall-method.md)

[`chmUninstall()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmUninstall-method.md)

## Examples

``` r
cloudServ <- chmNewServer("dnsname.domain")
```
