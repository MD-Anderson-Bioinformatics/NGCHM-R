\name{chmListTypes}
\alias{chmListTypes}
\title{List known axis types.}
\usage{
  chmListTypes(re = ".*")
}
\arguments{
  \item{re}{Only types with names matching re are returned
  (default ".*")}
}
\value{
  a character vector of axis type names
}
\description{
  This function returns a list of the axis types for which
  axis- or matrix- menu entries may be defined.
}
\seealso{
  chmAddAxisType
}

