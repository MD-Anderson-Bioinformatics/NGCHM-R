\name{chmAddAxisType}
\alias{chmAddAxisType}
\alias{chmAddAxisType,ngchm,character,character,character-method}
\alias{chmAddAxisType,ngchm,character,character,missing-method}
\alias{chmAddAxisType,ngchm,character,character,ngchmJS-method}
\title{Add an axis type to a NGCHM.}
\usage{
  chmAddAxisType(chm, where, type, func)
}
\arguments{
  \item{chm}{The chm to add the axis type to.}

  \item{where}{The axis to add the axis type to. Must be
  either "row" or "column".}

  \item{type}{The type to add to the specified axis.}

  \item{func}{A javascript function that gets values of
  that type from the current selection.  If a string is
  provided, the function is obtained by calling
  chmGetFunction.}
}
\value{
  The extended chm.
}
\description{
  Adds an axis type to a Next Generation Clustered Heat Map
  (NGCHM) and returns the extended CHM.  Multiple axis
  types may be added to either axis. When the NGCHM is
  made, any Axis functions matching the specified axis type
  will be automatically added to the appropriate axis menu,
  and any Matrix functions matching the types of the rows
  and columns will be automatically added to the matrix
  menu.
}
\seealso{
  chmListTypes

  chmRegisterAxisFunction

  chmRegisterMatrixFunction

  chmRegisterTypeMapper

  ngchmAxisType-class
}

