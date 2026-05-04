# Set the user's current server and/or collection

The path is a sequence of components separated by slashes (/). If the
path begins with a double slash (//) the following component is
interpreted as a server name. If the server name is omitted (i.e. empty)
the default server will be used. If the path does not begin with a
double slash, the current server will be used.

## Usage

``` r
chmSetCollection(path)
```

## Arguments

- path:

  A single character string specifying the path of the collection to be
  set. The path should be in the format '//server/collection'.

## Value

None. This function is used for its side effects of setting the current
server and collection.

## Details

If the path begins with a slash, the components (following the server,
if specified) are interpreted relative to the root collection of the
server concerned. Otherwise, they are interpreted relative to the
current collection.

The interpretation of each path component is server specific.

## See also

[`chmCurrentCollection()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmCurrentCollection.md)

[`chmServer()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmServer.md)

[`chmListServers()`](https://md-anderson-bioinformatics.github.io/NGCHM-R/reference/chmListServers.md)
