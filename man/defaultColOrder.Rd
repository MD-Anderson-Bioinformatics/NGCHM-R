\name{defaultColOrder}
\alias{defaultColOrder}
\title{Create a new NGCHM.}
\usage{
  defaultColOrder(chm)
}
\arguments{
  \item{name}{The name under which the NGCHM will be saved
  to the NGCHM server.}
}
\value{
  An object of class ngchm
}
\description{
  This function creates a Next Generation Clustered Heat
  Map (NGCHM) object in memory. Additional parameters will
  be added to the new NGCHM (see chmAdd). The bare NGCHM
  needs at least one data layer added to it before it can
  be compiled.
}
\examples{
mychm <- chmNew ("test_chm")
}
\seealso{
  ngchm-class

  ngchmServer-class

  chmAdd

  chmMake

  chmInstall
}

