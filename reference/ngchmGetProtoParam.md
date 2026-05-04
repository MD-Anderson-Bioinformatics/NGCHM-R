# Get Protocol Parameter for NG-CHM Server

This function gets a protocol parameter for a specified NG-CHM
(Next-Generation Clustered Heat Map) server.

## Usage

``` r
ngchmGetProtoParam(server, option, default = NULL)
```

## Arguments

- server:

  An object of class 'ngchmServer' representing the server.

- option:

  A single character string specifying the name of the protocol
  parameter.

- default:

  An optional default value to return if the protocol parameter is not
  found. Defaults to NULL.

## Value

The value of the protocol parameter if it is found, otherwise the
specified default value.
