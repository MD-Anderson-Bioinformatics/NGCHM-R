# Open the NG-CHM Manager

This function opens a web browser to view the NG-CHM (Next-Generation
Clustered Heat Map) Manager on the specified server.

## Usage

``` r
chmManager(server = NULL, viewer = NULL)
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
browser to view the NG-CHM Manager.

## See also

[`utils::browseURL()`](https://rdrr.io/r/utils/browseURL.html)
