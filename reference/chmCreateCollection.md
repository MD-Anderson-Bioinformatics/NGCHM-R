# Create a new NG-CHM Collection

This function creates a new NG-CHM (Next-Generation Clustered Heat Map)
collection on the server.

## Usage

``` r
chmCreateCollection(path, recursive = FALSE)
```

## Arguments

- path:

  The path where the collection should be created. This should be a
  single character string.

- recursive:

  A logical value indicating whether to create parent collections if
  they do not exist. Default is FALSE.

## Value

None. This function is used for its side effects of creating a new
collection on the server.

## Details

The path is a sequence of components separated by slashes (/). If the
path begins with a double slash (//) the following component is
interpreted as a server name. If the server name is omitted (i.e. empty)
the default server will be used. If the path does not begin with a
double slash, the current server will be used.

If the path begins with a slash, the components (following the server,
if specified) are interpreted relative to the root collection of the
server concerned. Otherwise, they are interpreted relative to the
current collection.

The interpretation of each path component is server specific.

## See also

[`chmCurrentCollection()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmCurrentCollection.md)
