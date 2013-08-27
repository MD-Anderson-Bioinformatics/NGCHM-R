\name{chmAddDataset}
\alias{chmAddDataset}
\alias{chmAddDataset,ngchm,ngchmDataset-method}
\title{Add an auxiliary dataset to a NGCHM.}
\usage{
  chmAddDataset(chm, dataset)
}
\arguments{
  \item{chm}{The chm to add the dataset to.}

  \item{dataset}{The dataset to add to the chm.}
}
\value{
  The extended chm.
}
\description{
  Add an auxiliary dataset to a Next Generation Clustered
  Heat Map (NGCHM) and return the extended CHM. The
  auxiliary dataset will be stored with the NGCHM and be
  available in whole or in part from the same server, for
  use, for example, in custom Javascript functions. Do not
  confuse this function with the one for adding an active
  data layer to the heatmap itself. For that, please refer
  to the function chmAddLayer.
}
\seealso{
  chmNewDataset

  ngchmDataset-class
}

