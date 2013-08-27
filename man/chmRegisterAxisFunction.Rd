\name{chmRegisterAxisFunction}
\alias{chmRegisterAxisFunction}
\title{Register a predefined Javascript function for use in NGCHM Axis menus.}
\usage{
  chmRegisterAxisFunction(type, label, fn)
}
\arguments{
  \item{type}{The axis type required by this function.}

  \item{label}{The name of the axis menu entry to be used
  for this function.}

  \item{fn}{The Javascript function to register.}
}
\value{
  NULL
}
\description{
  This function registers a Javascript function that will
  be automatically added to the appropriate axis menu(s)
  when building a Next Generation Clustered Heat Map for
  axes that match the function's axis type. This function
  is intended for use by NGCHM system developers.
}
\seealso{
  chmAddAxisType

  chmRegisterMatrixFunction

  chmRegisterTypeMapper

  chmNewFunction
}

