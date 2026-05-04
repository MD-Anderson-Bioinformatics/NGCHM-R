# Set number of characters to display for row or column labels

Sets the maximum number of characters to display for row or column
labels in the NG-CHM viewer.

## Usage

``` r
chmSetDisplayLength(chm, displayLength, rowOrCol)

# S4 method for class 'ngchmVersion2'
chmSetDisplayLength(chm, displayLength, rowOrCol)
```

## Arguments

- chm:

  An ngchmVersion2 object to modify

- displayLength:

  Numeric value to set as display length (allowed range: 1 to 99,
  inclusive)

- rowOrCol:

  Character string indicating which labels to modify: "row" or "col"
  ("column" also accepted)

## Value

Modified ngchmVersion2 object with updated display length

## Examples

``` r
# Create a new NG-CHM object
chm <- chmNew("New Heat Map")
# Set row labels to display up to 20 characters
chm <- chmSetDisplayLength(chm, 20, "row")
# Set column labels to display up to 15 characters
chm <- chmSetDisplayLength(chm, 15, "col")
```
