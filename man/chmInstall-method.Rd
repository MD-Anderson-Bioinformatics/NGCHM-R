\name{chmInstall}
\alias{chmInstall}
\alias{chmInstall,ngchmServer,ngchm-method}
\title{Install a compiled NGCHM on a server.}
\usage{
  chmInstall(server, chm)
}
\arguments{
  \item{server}{The server on which to install the NGCHM.}

  \item{chm}{The NGCHM to install.}
}
\description{
  Install the specified Next Generation Clustered Heat Map
  (NGCHM) on the specified server.  The installed NGCHM is
  public by default.
}
\seealso{
  ngchmServer-class

  ngchm-class

  chmUninstall

  chmMakePrivate

  chmMakePublic
}

