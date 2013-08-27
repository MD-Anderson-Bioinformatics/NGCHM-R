\name{chmGetFunction}
\alias{chmGetFunction}
\title{Get a predefined Javascript function for use in NGCHM menus}
\usage{
  chmGetFunction(name)
}
\arguments{
  \item{name}{The name of the predefined Javascript
  function desired.}
}
\value{
  An object of class ngchmFunction if found, NULL
  otherwise.
}
\description{
  This function returns a predefined Javascript function
  that can be used when building a Next Generation
  Clustered Heat Map.
}
\seealso{
  chmAddMenuItem

  chmNewFunction

  ngchmFunction-class
}

