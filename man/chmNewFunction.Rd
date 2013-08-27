\name{chmNewFunction}
\alias{chmNewFunction}
\title{Create a new Javascript function for adding to a NGCHM menu.}
\usage{
  chmNewFunction(name, description, implementation, global)
}
\arguments{
  \item{name}{The name of the Javascript function}

  \item{description}{A short description of the Javascript
  function}

  \item{implementation}{A string containing the javascript
  code required to define the function.}

  \item{global}{A logical: TRUE if should be defined
  globally, not within a customization section.}
}
\value{
  An object of class ngchmJS
}
\description{
  This function creates a new Javascript function object
  for adding to a Next Generation Clustered Heat Map menu.
}
\examples{
alertFn <- chmNewFunction ("showAlert", "Display the parameter in an alert box",
                           "function showAlert(label) { alert(label); }", TRUE)
}
\seealso{
  ngchmJS-class

  chmAddMenuItem

  chmRegisterFunction
}

