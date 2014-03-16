\name{chmAddCovariate}
\alias{chmAddCovariate}
\alias{chmAddCovariate,ngchmDataset,character,ngchmCovariate-method}
\title{Add a covariate to an auxiliary dataset.}
\usage{
  chmAddCovariate(dataset, where, cov)
}
\arguments{
  \item{dataset}{The dataset to add the covariate to.}

  \item{where}{The dataset axis to add the covariate to.
  Must be one of "row", "column", or "both".}

  \item{cov}{The covariate to add to the dataset.}
}
\value{
  The extended dataset.
}
\description{
  Add a covariate to an auxiliary dataset and return the
  extended dataset. Do not confuse this function with the
  one for adding a classification bar to an NGCHM. For
  that, please refer to the function chmAddClassBar.
}
\seealso{
  chmNewCovariate

  ngchmCovariate-class
}

