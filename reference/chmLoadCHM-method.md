# Load CHM from NG-CHM server

Load an R CHM object from an NG-CHM server. The CHM concerned must have
been built using this library, version 0.9.4 or later.

## Usage

``` r
chmLoadCHM(serverOrURL, name)

# S4 method for class 'ngchmServer,character'
chmLoadCHM(serverOrURL, name)

# S4 method for class 'character,character'
chmLoadCHM(serverOrURL, name)

# S4 method for class 'character,missing'
chmLoadCHM(serverOrURL, name)
```

## Arguments

- serverOrURL:

  An object of class 'ngchmServer' representing the server from which
  the NG-CHM is to be loaded.

- name:

  A single character string specifying the name of the NG-CHM to be
  loaded.

## Value

An object of class 'ngchm' representing the loaded NG-CHM.
