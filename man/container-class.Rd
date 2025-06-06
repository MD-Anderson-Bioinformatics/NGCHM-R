% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panelClasses.R
\docType{class}
\name{container-class}
\alias{container-class}
\title{Container Class for Panel Layout}
\description{
The container class is part of the panel layout system in NG-CHM. It is used to organize
panes in a hierarchical structure. Each container can be either vertical or horizontal,
and can contain multiple children (panes or other containers).
}
\section{Slots}{

\describe{
\item{\code{children}}{A list of child objects (panes or other containers)}

\item{\code{height}}{Numeric value for container height (as percentage, 0-100)}

\item{\code{id}}{Character string identifying the container (e.g. "ngChmContainer1")}

\item{\code{vertical}}{Logical indicating container is vertical (TRUE) or horizontal (FALSE)}

\item{\code{width}}{Numeric value for container width (as percentage, 0-100)}
}}

\seealso{
\itemize{
\item \link{pane} for leaf nodes in the layout tree
}
}
