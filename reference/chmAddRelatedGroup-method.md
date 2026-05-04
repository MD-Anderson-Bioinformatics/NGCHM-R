# Add a group of related links to the NGCHM.

Add a group of related links to the NGCHM.

## Usage

``` r
chmAddRelatedGroup(chm, name, header, linktype, blurb)

# S4 method for class 'ngchm,character,character,character,character'
chmAddRelatedGroup(chm, name, header, linktype, blurb)

# S4 method for class 'ngchm,character,character,character,missing'
chmAddRelatedGroup(chm, name, header, linktype)
```

## Arguments

- chm:

  The chm to add the related link group to.

- name:

  The name of the group of links.

- header:

  The header that should be displayed for this group of links.

- linktype:

  Type of link belonging to this group.

- blurb:

  An optional descriptive paragraph to include between the group header
  and the group links.

## Value

The extended chm.
