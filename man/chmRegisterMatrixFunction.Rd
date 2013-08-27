\name{chmRegisterMatrixFunction}
\alias{chmRegisterMatrixFunction}
\title{Register a predefined Javascript function for use in NGCHM Matrix menus.}
\usage{
  chmRegisterMatrixFunction(rowtype, columntype, label, fn)
}
\arguments{
  \item{rowtype}{The row type required by this function.}

  \item{columntype}{The column type required by this
  function.}

  \item{label}{The name of the axis menu entry to be used
  for this function.}

  \item{fn}{The Javascript function to register.}
}
\value{
  NULL
}
\description{
  This function registers a Javascript function that will
  be automatically added to the matrix menu when building a
  Next Generation Clustered Heat Map for matrices whose
  rows and columns match then function's axes types. This
  function is intended for use by NGCHM system developers.
}
\seealso{
  chmAddAxisType

  chmRegisterAxisFunction

  chmRegisterTypeMapper

  chmNewFunction
}

