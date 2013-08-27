\docType{package}
\name{NGCHM}
\alias{NGCHM}
\alias{NGCHM-package}
\title{Next Generation Clustered Heat Map (NGCHM) Construction Library}
\description{
  NGCHM provides tools for defining the contents of a new
  NGCHM, and for compiling and installing it on a NGCHM
  server.  This package is often supplemented by a local
  package that predefines the available NGCHM servers and
  provides a library of Javascript functions for adding to
  label and element popup menus.
}
\details{
  Typical usage is to create a base NGCHM using chmNew;
  extend it with at least one ngchmLayer; typically extend
  it further with an additional ngchmLayer, row and column
  dendrograms, classification bars, and popup menu entries;
  compile and install it on an available ngchmServer.
}
\seealso{
  ngchm-class

  ngchmServer-class
}

