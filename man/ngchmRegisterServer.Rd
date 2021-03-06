% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{ngchmRegisterServer}
\alias{ngchmRegisterServer}
\title{Register an ngchmServer.}
\usage{
ngchmRegisterServer(uuid, server)
}
\arguments{
\item{uuid}{A string that identifies the server namespace.}

\item{server}{The ngchmServer to register.}
}
\value{
the server that was registered
}
\description{
This function registers an ngchmServer that can be used when
making and installing a Next Generation Clustered Heat Map.
}
\seealso{
\code{\link[=chmInstall]{chmInstall()}}

\code{\link[=chmUninstall]{chmUninstall()}}

\code{\link[=ngchmUnregisterServer]{ngchmUnregisterServer()}}

\linkS4class{ngchmServer}
}
