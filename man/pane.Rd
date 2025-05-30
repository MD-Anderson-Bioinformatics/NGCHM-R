% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panelClasses.R
\name{pane}
\alias{pane}
\title{Create a pane for NG-CHM layout}
\usage{
pane(id = NA, width = 100, height = 100, collapsed = FALSE, expanded = FALSE)
}
\arguments{
\item{id}{Character string identifying the pane. Must be specified, no default.}

\item{width}{Numeric value for pane width, expressed as a percentage (0-100). Default is 100.}

\item{height}{Numeric value for pane height, expressed as a percentage (0-100). Default is 100.}

\item{collapsed}{Logical indicating if the pane is collapsed. Default is FALSE.}

\item{expanded}{Logical indicating if the pane is expanded. Default is FALSE.}
}
\value{
A new \code{pane} object with the specified properties
}
\description{
Creates a new pane object representing a single viewing area in the NG-CHM panel layout system.
}
\examples{
# Create a basic pane
p1 <- pane(id = "pane1")

# Create a half-width pane
p2 <- pane(id = "pane2", width = 50)

# Create an expanded pane
p3 <- pane(id = "pane3", expanded = TRUE)

}
\seealso{
\itemize{
\item \linkS4class{pane} for class details
\item \link{panel_configuration} for using panes in configurations
}
}
