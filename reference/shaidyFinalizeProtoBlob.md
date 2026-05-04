# Finalize a prototype blob

Finalize a prototype blob

## Usage

``` r
shaidyFinalizeProtoBlob(shaidyRepo, shaid, protoblob)
```

## Arguments

- shaidyRepo:

  The shaidy repository

- shaid:

  The shaid to assign the protoblob

- protoblob:

  The prototype blob to finalize

## Value

The shaid (invisibly)

The protoblob must have been created in the specified shaidy repository
and with the same blob type as the shaid. When this function returns the
protoblob will no longer be accessible . If a blob with the same shaid
already exists in this repository, the protoblob is quitely removed
without affecting the existing blob.
