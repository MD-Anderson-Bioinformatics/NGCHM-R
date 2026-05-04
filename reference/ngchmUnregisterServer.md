# Unregister NG-CHM Server

This function unregisters a server for NG-CHM (Next-Generation Clustered
Heat Map) by its UUID and optionally by its name.

## Usage

``` r
ngchmUnregisterServer(uuid, name = NULL)
```

## Arguments

- uuid:

  A single character string specifying the UUID of the server.

- name:

  The names(s) of the ngchmServer(s) to unregister. If not specified,
  all ngchmServers in the namespace are unregistered. Defaults to NULL.

## Value

None. This function is used for its side effects of unregistering the
server.

## See also

[`ngchmRegisterServer()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmRegisterServer.md)

[ngchmServer](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/ngchmServer-class.md)
