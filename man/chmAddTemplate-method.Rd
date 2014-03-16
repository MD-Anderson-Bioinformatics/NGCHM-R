\name{chmAddTemplate}
\alias{chmAddTemplate}
\alias{chmAddTemplate,ngchm,charOrFunction,character,optList-method}
\title{Add a file template to the NGCHM.}
\usage{
  chmAddTemplate(chm, source.path, dest.path,
    substitutions)
}
\arguments{
  \item{chm}{The chm to add the file template to.}

  \item{source.path}{A string giving the path to the
  template, or a function that returns the template content
  as a string.}

  \item{dest.path}{A string giving the relative path where
  to store the template in the generated CHM.}

  \item{substitutions}{A list (may be empty) of
  substitutions to make in the template.}
}
\value{
  The extended chm.
}
\description{
  Add a file template to the NGCHM.
}

