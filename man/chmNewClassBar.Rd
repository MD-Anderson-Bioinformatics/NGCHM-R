\name{chmNewClassBar}
\alias{chmNewClassBar}
\title{Create a new Classification Bar for a NGCHM}
\usage{
  chmNewClassBar(label, type, data, colors = NULL,
    display = "visible", thickness = as.integer(10),
    merge = NULL)
}
\arguments{
  \item{label}{The name by which the classification bar
  will be known.}

  \item{type}{The string "discrete" or the string
  "continuous".}

  \item{data}{A vector of the data to be displayed in the
  classification bar. names(data) must be defined.}

  \item{colors}{A color map specifying how the data should
  be rendered.}

  \item{display}{Whether the classification bar will be
  "hidden" or "visible" (default).}

  \item{thickness}{The thickness of the classification bar
  in pixels. (Default 10).}

  \item{merge}{Algorithm for merging classifications when
  necessary ("average", "peakColor", "specialColor", or
  "mostCommon").}
}
\value{
  An object of class ngchmBar
}
\description{
  This function creates a new Classification Bar suitable
  for adding to a Next Generation Clustered Heat Map.
}
\examples{
bar.data <- ifelse (rnorm(1000) < 0, "negative", "non-negative")
bar.colors <- chmNewColorMap ("linear", "red",
                              c("negative", "non-negative"),
                              c("white", "black"))
bar <- chmNewClassBar ("Group", "discrete", bar.data, bar.colors)
}
\seealso{
  ngchmBar-class

  chmNewColorMap

  chmAddClassBar
}

