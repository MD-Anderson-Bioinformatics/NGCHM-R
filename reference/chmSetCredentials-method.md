# Set Access Credentials for NG-CHM Server

This function sets the credentials for a specific NG-CHM
(Next-Generation Clustered Heat Map) server.

## Usage

``` r
chmSetCredentials(resource, credentials)

# S4 method for class 'ngchmServer,character'
chmSetCredentials(resource, credentials)

# S4 method for class 'character,character'
chmSetCredentials(resource, credentials)
```

## Arguments

- resource:

  An object of class 'ngchmServer' or a character string representing
  the server for which the credentials are to be set.

- credentials:

  A single character string specifying the credentials to be set for the
  server.

## Value

No return value. The function is called for its side effect of setting
the credentials for the specified server.
