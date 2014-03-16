\name{chmNewCovariate}
\alias{chmNewCovariate}
\title{Create a new Covariate for adding to an NGCHM auxilary dataset.}
\usage{
  chmNewCovariate(label, fullname, label.series,
    series.properties)
}
\arguments{
  \item{label}{The short label used to identify the
  covariate within its dataset.}

  \item{fullname}{The full (human readable) name of the
  covariate.}

  \item{label.series}{The series to which each label in the
  dataset belongs.}

  \item{column.series.properties}{A list of mappings from
  series names to property values.}
}
\value{
  An object of class ngchmCovariate.
}
\description{
  This function creates a new Covariate suitable for
  attaching to an NGCHM auxilary dataset.
}
\seealso{
  ngchmCovariate-class

  chmAddCovariate
}

