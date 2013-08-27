\name{chmColMeta<-}
\alias{chmColMeta<-}
\title{Set the column meta data.}
\usage{
  chmColMeta(chm) <- value
}
\arguments{
  \item{chm}{The chm for which to set the meta data.}

  \item{value}{A character vector or NULL If value is NULL,
  no meta data is associated with the NGCHM's columns. If
  value is a character vector, elements of the vector will
  be attached as meta data to to NGCHM column of the same
  name.}
}
\description{
  Set the meta data attached to columns in a Next
  Generation Clustered Heat Map.
}
\seealso{
  "chmRowMeta<-"
}

