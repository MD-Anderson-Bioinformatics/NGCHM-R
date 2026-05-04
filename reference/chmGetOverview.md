# Get the file path to the specified overview file.

This function returns the file path to the specified overview image of
the CHM. The CHM must be made before the file can be accessed. If idx is
specified, format if given must equal that of the overview image, and
the path to that overview image is returned. If idx is not specified,
the file path to the first overview of the given format (default 'png')
is returned.

## Usage

``` r
chmGetOverview(chm, format = NULL, idx = NULL)
```

## Arguments

- chm:

  The CHM for which the overview is to be retrieved.

- format:

  The format of overview image desired (defaults to 'png' if idx is not
  specified).

- idx:

  The index of the overview image desired (defaults to first image of
  the specified format).

## Value

The path to the retrieved overview.
