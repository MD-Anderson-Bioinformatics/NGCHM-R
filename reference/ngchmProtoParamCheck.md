# Check Protocol Parameters for NG-CHM

Check that all required parameters are specified, and all specified
parameters are either required or optional.

## Usage

``` r
ngchmProtoParamCheck(params, required, optional)
```

## Arguments

- params:

  A list of parameters to be checked.

- required:

  A character vector specifying the required parameters.

- optional:

  A character vector specifying the optional parameters.

## Value

None. This function is used for its side effects of checking the
parameters and potentially stopping execution with an error message.
