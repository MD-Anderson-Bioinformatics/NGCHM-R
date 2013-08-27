\name{chmAddRelated}
\alias{chmAddRelated}
\alias{chmAddRelated,ngchm,character,character,character-method}
\alias{chmAddTemplate}
\title{Add a link to related information to the NGCHM.}
\usage{
  chmAddRelated(chm, group, link, description)

  chmAddTemplate(chm, source.path, dest.path,
    substitutions)
}
\arguments{
  \item{chm}{The chm to add the related link to.}

  \item{group}{The name of the group this link belongs to.}

  \item{link}{The link to include. Should be either an
  absolute URL, or a NGCHM name on the same server.}

  \item{description}{A string describing the referenced
  link and its relationship to the current NGCHM.}

  \item{chm}{The chm to add the file template to.}

  \item{source.path}{A string giving the path to the
  template.}

  \item{dest.path}{A string giving the relative path where
  to store the template in the generated CHM.}

  \item{substitutions}{A list (may be empty) and
  substitutions to make in the template.}
}
\value{
  The extended chm.

  The extended chm.
}
\description{
  Add a link to related information to the NGCHM.

  Add a file template to the NGCHM.
}

