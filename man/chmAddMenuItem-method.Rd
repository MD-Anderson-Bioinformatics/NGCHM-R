% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R, R/allMethods.R
\name{chmAddMenuItem}
\alias{chmAddMenuItem}
\alias{chmAddMenuItem,ngchm,character,character,ngchmJS-method}
\alias{chmAddMenuItem,ngchm,character,character,character-method}
\title{Add a menu entry to a NGCHM.}
\usage{
chmAddMenuItem(chm, where, label, func)

\S4method{chmAddMenuItem}{ngchm,character,character,ngchmJS}(chm, where, label, func)

\S4method{chmAddMenuItem}{ngchm,character,character,character}(chm, where, label, func)
}
\arguments{
\item{chm}{The chm to add the menu entry to.}

\item{where}{The chm menu(s) to add the menu entry to. Must be one of "row", "column", "both", or "element".}

\item{label}{The label to display in the menu entry.}

\item{func}{The javascript function to invoke when the menu entry is selected.}
}
\value{
The extended chm.
}
\description{
Add a popup menu entry to a Next Generation Clustered Heat Map (NGCHM) and
return the extended CHM.
}
\seealso{
\linkS4class{ngchmMenuItem}
}
