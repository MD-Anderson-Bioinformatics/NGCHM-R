\name{chmNewDataset}
\alias{chmNewDataset}
\title{Create a new Dataset for a NGCHM.}
\usage{
  chmNewDataset(name, description, data,
    row.properties = NULL, column.properties = NULL)
}
\arguments{
  \item{name}{The filename prefix under which the dataset
  will be saved to the ngchm.}

  \item{description}{A description of the dataset.}

  \item{data}{A matrix containing the data in the dataset.
  Must have rownames and colnames.}

  \item{row.properties}{An optional list of mappings from
  row labels to property values.}

  \item{column.properties}{An optional list of mappings
  from column labels to property values.}
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

  chmAddDataset
}

