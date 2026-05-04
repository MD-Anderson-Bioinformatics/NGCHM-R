# Add data file(s) and properties to a local shaidy repository

Add data file(s) and properties to a local shaidy repository

## Usage

``` r
shaidyAddFileBlob(
  shaidyRepo,
  blob.type,
  blob.file,
  filename,
  properties = NULL,
  shaid = NULL
)
```

## Arguments

- shaidyRepo:

  The shaidy repository

- blob.type:

  The blob.type of the data file

- blob.file:

  Name of the file(s) within the blob

- filename:

  The filesystem path(s) to the file(s) to insert

- properties:

  A list of additional properties to save with the file(s)

- shaid:

  Shaid to store the blob as.

## Value

The file's shaid
