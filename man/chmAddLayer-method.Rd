\name{chmAddLayer}
\alias{chmAddLayer}
\alias{chmAddLayer,ngchm,matrix-method}
\title{Add a Layer to a NGCHM.}
\usage{
  chmAddLayer(chm, layer)
}
\arguments{
  \item{chm}{The chm to add the layer to.}

  \item{layer}{The layer to add to the chm.}
}
\value{
  The extended chm.
}
\description{
  Add a Layer to a Next Generation Clustered Heat Map
  (NGCHM) and return the extended CHM.  A CHM requires at
  least one Layer.  The first layer added to a NGCHM
  becomes the primary layer. The second layer added to a
  NGCHM, if any, becomes the secondary (flicker) layer.
  Currently at most two layers can be added to a NGCHM.
}
\seealso{
  chmNewDataLayer

  ngchmLayer-class
}

