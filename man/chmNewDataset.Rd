\name{chmNewDataset}
\alias{chmNewDataset}
\title{Create a new Dataset for a NGCHM.}
\usage{
  chmNewDataset(name, description, data,
    row.covariates = NULL, column.covariates = NULL)
}
\arguments{
  \item{name}{The filename prefix under which the dataset
  will be saved to the ngchm.}

  \item{description}{A description of the dataset.}

  \item{data}{A matrix containing the data in the dataset.
  Must have rownames and colnames.}

  \item{row.covariates}{An optional list of row
  covariates.}

  \item{column.covariates}{An optional list of column
  covariates.}
}
\value{
  An object of class ngchmDataset
}
\description{
  This function creates a new Dataset suitable for
  attaching to a Next Generation Clustered Heat Map.
}
\seealso{
  ngchmDataset-class

  ngchmCovariate-class

  chmAddDataset
}

