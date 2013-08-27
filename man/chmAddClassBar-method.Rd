\name{chmAddClassBar}
\alias{chmAddClassBar}
\alias{chmAddClassBar,ngchm,character,ngchmBar-method}
\title{Add a classification bar to a NGCHM.}
\usage{
  chmAddClassBar(chm, where, bar)
}
\arguments{
  \item{chm}{The chm to add the classification bar to.}

  \item{where}{The chm axis(axes) to add the classification
  bar to. Must be one of "row", "column", or "both".}

  \item{bar}{The classification bar to add to the chm.}
}
\value{
  The extended chm.
}
\description{
  Add a classification bar to a Next Generation Clustered
  Heat Map (NGCHM) and return the extended CHM.
}
\seealso{
  chmNewClassBar

  ngchmBar-class
}

