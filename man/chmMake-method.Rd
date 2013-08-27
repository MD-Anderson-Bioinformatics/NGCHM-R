\name{chmMake}
\alias{chmAddLayer}
\alias{chmAddLayer,ngchm,ngchmLayer-method}
\alias{chmMake}
\alias{chmMake,ngchmServer,ngchm,missing-method}
\alias{chmMake,ngchmServer,ngchm-method}
\title{Compile a NGCHM.}
\usage{
  chmMake(server, chm, deleteOld, useJAR)
}
\arguments{
  \item{server}{The server for which to compile the NGCHM.}

  \item{chm}{The NGCHM to compile.}

  \item{deleteOld}{If TRUE, delete any old CHM of this name
  before beginning build. (Default is TRUE.)}

  \item{useJAR}{If defined, the location (filename) of the
  chmbuilder jar file. The package will not download a
  current jar file from the server. It is the caller's
  responsibility to ensure the builder jar file is
  compatible with the server on which the NGCHM will be
  installed. (Default is not defined.)}
}
\description{
  Compile the specified Next Generation Clustered Heat Map
  (NGCHM) in preparation for installation on the specified
  server.
}
\seealso{
  ngchmServer-class

  ngchm-class

  chmInstall
}

