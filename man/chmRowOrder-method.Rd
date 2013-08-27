\name{chmRowOrder<-}
\alias{chmRowOrder<-}
\title{Set the row order of data shown in a NGCHM.}
\usage{
  chmRowOrder(chm) <- value
}
\arguments{
  \item{chm}{The chm for which to set the row order.}

  \item{value}{An ordering of the row labels. If value is
  NULL, the labels will be displayed in the same order they
  are found in the first data layer. If value is a
  character vector, the labels will be displayed in that
  order. If value is a dendrogram, the labels displayed in
  the order they occur in a depth first traversal of the
  tree.}
}
\description{
  Set the order in which rows in the data will be shown in
  a Next Generation Clustered Heat Map.
}
\seealso{
  "chmColOrder<-"
}

