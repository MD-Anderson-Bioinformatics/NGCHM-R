# Add a link to related information to the NGCHM.

Add a link to related information to the NGCHM.

## Usage

``` r
chmAddRelated(chm, group, link, description)

# S4 method for class 'ngchm,character,character,character'
chmAddRelated(chm, group, link, description)
```

## Arguments

- chm:

  The chm to add the related link to.

- group:

  The name of the group this link belongs to.

- link:

  The link to include. Should be either an absolute URL, or a NGCHM name
  on the same server.

- description:

  A string describing the referenced link and its relationship to the
  current NGCHM.

## Value

The extended chm.
