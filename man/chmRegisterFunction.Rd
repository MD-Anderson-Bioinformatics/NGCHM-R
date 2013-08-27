\name{chmRegisterFunction}
\alias{chmRegisterFunction}
\title{Register a predefined Javascript function for use in NGCHM menus.}
\usage{
  chmRegisterFunction(fn)
}
\arguments{
  \item{fn}{The Javascript function to register.}
}
\value{
  NULL
}
\description{
  This function registers a Javascript function that can be
  used when building a Next Generation Clustered Heat Map.
  This function is intended for use by NGCHM system
  developers.
}
\note{
  The following functions are currently available by this
  library: \describe{ \item{showGeneCardPage}{Opens the
  GeneCards page for the gene concerned in the 'genecards'
  window.} \item{showNCBIGenePage}{Opens the NCBI page for
  the gene concerned in the 'NCBI' window.} } Other
  libraries may provide additional predefined functions.
}
\seealso{
  chmAddMenuItem

  chmNewFunction

  ngchmFunction-class
}

