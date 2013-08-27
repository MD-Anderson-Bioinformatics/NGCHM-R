\name{chmAddRelatedGroup}
\alias{chmAddRelatedGroup}
\alias{chmAddRelatedGroup,ngchm,character,character,character,character-method}
\alias{chmAddRelatedGroup,ngchm,character,character,character,missing-method}
\title{Add a group of related links to the NGCHM.}
\usage{
  chmAddRelatedGroup(chm, name, header, linktype, blurb)
}
\arguments{
  \item{chm}{The chm to add the related link group to.}

  \item{name}{The name of the group of links.}

  \item{header}{The header that should be displayed for
  this group of links.}

  \item{linktype}{Type of link belonging to this group.}

  \item{blurb}{An optional descriptive paragraph to include
  between the group header and the group links.}
}
\value{
  The extended chm.
}
\description{
  Add a group of related links to the NGCHM.
}

