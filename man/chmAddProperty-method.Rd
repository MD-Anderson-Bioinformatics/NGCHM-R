% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R, R/allMethods.R
\name{chmAddProperty}
\alias{chmAddProperty}
\alias{chmAddProperty,ngchm,character,character-method}
\title{Add custom property to a NGCHM.}
\usage{
chmAddProperty(chm, label, value)

\S4method{chmAddProperty}{ngchm,character,character}(chm, label, value)
}
\arguments{
\item{chm}{The chm to add the property to.}

\item{label}{The property label.}

\item{value}{The property value.}
}
\value{
The extended chm.
}
\description{
Add custom property to a Next Generation Clustered Heat Map (NGCHM) and
return the extended CHM.
}
\seealso{
\linkS4class{ngchmProperty}
}
