# Get per-user configuration for a specific deploy Server.

This function retrieves the configuration of a specified NG-CHM
(Next-Generation Clustered Heat Map) deployment server.

## Usage

``` r
chmGetDeployServerConfig(server)
```

## Arguments

- server:

  The server for which the configuration is to be retrieved. This can be
  either a character string representing the server name or an object of
  class 'ngchmServer'.

## Value

The configuration of the specified server if it exists, otherwise NULL.
