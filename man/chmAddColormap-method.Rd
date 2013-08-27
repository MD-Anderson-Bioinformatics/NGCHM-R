\name{chmAddColormap}
\alias{chmAddColormap}
\alias{chmAddColormap,ngchm,ngchmColormap-method}
\title{Add a colormap to a NGCHM.}
\usage{
  chmAddColormap(chm, colormap)
}
\arguments{
  \item{chm}{The chm to add the colormap to.}

  \item{colormap}{The colormap to add to the chm.}
}
\value{
  The extended chm.
}
\description{
  Add a colormap to a Next Generation Clustered Heat Map
  (NGCHM) and return the extended CHM. Duplicate colormaps
  will be silently dropped.
}
\details{
  Note that it is not necessary to explicitly add colormaps
  included with data layers or classification bars.  These
  will be included automatically.  Explicitly using this
  function is only required in order to add additional
  predefined, but unused colormaps to the NGCHM.
}
\seealso{
  chmNewColormap

  ngchmColormap-class
}

