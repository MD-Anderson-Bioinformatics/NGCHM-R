\name{chmMakePrivate}
\alias{chmMakePrivate}
\alias{chmMakePrivate,ngchmServer,character-method}
\alias{chmMakePrivate,ngchmServer,ngchm-method}
\title{Make an installed NGCHM private.}
\usage{
  chmMakePrivate(server, chm)
}
\arguments{
  \item{server}{The server on which the NGCHM to make
  private is installed.}

  \item{chm}{The NGCHM to make private.}
}
\description{
  Make private the specified Next Generation Clustered Heat
  Map (NGCHM) on the specified server.
}
\seealso{
  ngchmServer-class

  ngchm-class

  chmInstall

  chmUninstall

  chmMakePublic
}

