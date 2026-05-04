# Push a local shaidy repository onto the stack of source repositories

This function pushes a source repository for NG-CHM (Next-Generation
Clustered Heat Map) onto the Shaidy stack.

## Usage

``` r
ngchmPushSourceRepository(shaidyDir, accessMethod = "file")
```

## Arguments

- shaidyDir:

  A single character string specifying the directory of the source
  repository.

- accessMethod:

  A single character string specifying the access method for the source
  repository. Defaults to "file".

## Value

None. This function is used for its side effects of pushing the source
repository onto the Shaidy stack.
