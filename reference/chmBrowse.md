# Browse the NGCHMs on the specified server in the viewer.

Opens the NG-CHM browser page in the viewer.

## Usage

``` r
chmBrowse(server = NULL, viewer = NULL)
```

## Arguments

- server:

  The NG-CHM server to be browsed. If NULL, the function will use the
  first server in the list of available servers.

- viewer:

  The function to be used to open the web browser. If NULL, the function
  will use the 'browseURL' function.

## Value

None. This function is used for its side effects of opening a web
browser to view the NG-CHM server.

## See also

[`utils::browseURL()`](https://rdrr.io/r/utils/browseURL.html)
