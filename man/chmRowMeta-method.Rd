\name{chmRowMeta<-}
\alias{chmRowMeta<-}
\title{Set the row meta data.}
\usage{
  chmRowMeta(chm) <- value
}
\arguments{
  \item{chm}{The chm for which to set the meta data.}

  \item{value}{A character vector or NULL. If value is
  NULL, no meta data is associated with the NGCHM's rows.
  If value is a character vector, elements of the vector
  will be attached as meta data to to NGCHM row of the same
  name.}
}
\description{
  Set the meta data attached to rows in a Next Generation
  Clustered Heat Map.
}
\seealso{
  "chmColMeta<-"
}

