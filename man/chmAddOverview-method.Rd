\name{chmAddOverview}
\alias{chmAddOverview}
\alias{chmAddOverview,ngchm,character,numeric,numeric-method}
\title{Generate an overview image of the NGCHM when making it.}
\usage{
  chmAddOverview(chm, format, width, height)
}
\arguments{
  \item{chm}{The chm to add the overview to.}

  \item{format}{The format of the overview (either 'pdf' or
  'png').}

  \item{width}{The width of the overview.}

  \item{height}{A height of the overview.}
}
\value{
  The extended chm.
}
\description{
  Generate an overview image of the NGCHM when making it.
  By default, the system generates two default overview
  images: a PDF image and a PNG image.  The first two
  overviews you add to the NGCHM will override the default
  settings for these overviews.  If only one of width or
  height is specified, the other is calculate based on the
  aspect ratio of the map.
}

