\name{chmRegisterToolboxFunction}
\alias{chmRegisterToolboxFunction}
\title{Register a Javascript function for use in the NGCHM toolbox.}
\usage{
  chmRegisterToolboxFunction(tbtype, menulabel, jsfn)
}
\arguments{
  \item{tbtype}{The toolbox type.}

  \item{menulabel}{The base menu label for the toolbox
  operation.}

  \item{jsfn}{The Javascript function that implements the
  toolbox operation.}
}
\value{
  NULL
}
\description{
  This function registers a Javascript function that can
  included in the toolbox of an NGCHM.  This function is
  intended for use by NGCHM system developers.
}
\seealso{
  chmNewFunction

  ngchmFunction-class
}

