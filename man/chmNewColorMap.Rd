\name{chmNewColorMap}
\alias{chmNewColorMap}
\title{Create a new Color Map for use in constructing a NGCHM}
\usage{
  chmNewColorMap(type, missing, values, colors)
}
\arguments{
  \item{type}{The string "linear" or the string "quantile"
  (or unique abbreviation thereof).}

  \item{missing}{A string specifying the color to use for
  missing data.}

  \item{values}{A vector specifying the break points for
  which the following colors are defined, or a data
  matrix.}

  \item{colors}{A vector of strings specifying the color to
  use for each color break point.}
}
\value{
  An object of class ngchmColormap
}
\description{
  This function creates a new Color Map suitable for use in
  constructing Data Layers and Classification Bars in Next
  Generation Clustered Heat Map. If values is a matrix, the
  function will estimate a suitable sequence of color break
  points.  For a quantile color map, the matrix data is
  ignored.  For a linear color map, it will use equispaced
  values between a low value and a high value. The low
  value is the median of the minima of each row in the
  matrix, and the high value is the median of the row
  maxima. If the low and high values have different signs,
  the values will be symmetric about zero.
}
\examples{
noise.colors <- chmNewColorMap ("linear", "yellow", c(0,1,2),
                                c("green", "black", "red"))
bar.colors <- chmNewColorMap ("quantile", "white", c("small", "big"),
                              c("#00FFFF", "#FF00FF"))
}
\seealso{
  ngchmColormap-class

  chmNewDataLayer

  chmNewClassBar
}

