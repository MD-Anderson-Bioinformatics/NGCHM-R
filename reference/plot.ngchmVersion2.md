# Open the NG-CHM on the specified server in the viewer.

Open the NG-CHM on the specified server in the viewer.

## Usage

``` r
# S3 method for class 'ngchmVersion2'
plot(x, server = NULL, viewer = NULL, ...)
```

## Arguments

- x:

  The NGCHM to view.

- server:

  The server containing the NG-CHM. Defaults to option "NGCHM.Server" or
  the first server.

- viewer:

  The viewer to use. Defaults to option "viewer" or browseURL.

- ...:

  Ignored.

## Value

No return value. The function is called for its side effect of plotting
the specified NG-CHM.
