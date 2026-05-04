# Push a shaidy server onto the stack of source repositories

This function pushes a source server for NG-CHM (Next-Generation
Clustered Heat Map) onto the Shaidy stack.

## Usage

``` r
ngchmPushSourceServer(server)
```

## Arguments

- server:

  An object of class 'ngchmServer' or a single character string
  specifying the name of the server.

## Value

None. This function is used for its side effects of pushing the source
server onto the Shaidy stack.

## See also

[`chmLoadShaidyCHM()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmLoadShaidyCHM.md)

[`chmCreateServer()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmCreateServer.md)
