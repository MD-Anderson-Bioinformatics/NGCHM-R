# Find a repository, if any, that contains the requested shaid

Find a repository, if any, that contains the requested shaid

## Usage

``` r
ngchmFindRepo(shaid, required = TRUE)
```

## Arguments

- shaid:

  The shaid to search for

- required:

  Abort if requireed and shaid not found in a known repo

## Value

The first repository containing the shaid, otherwise NULL. The temporary
repositories are searched before source repositories.
