\name{chmBindFunction}
\alias{chmBindFunction}
\alias{chmBindFunction,character,character,list-method}
\alias{chmBindFunction,character,ngchmJS,list-method}
\title{Bind values to an existing JS function.}
\usage{
  chmBindFunction(name, fn, bindings)
}
\arguments{
  \item{name}{The name to give the new JS function.}

  \item{fn}{An existing JS function with at least one extra
  parameter.}

  \item{bindings}{A list containing at least one parameter
  binding.  Each list element binds one parameter, starting
  from the first unbound parameter, and the name of each
  list element must match the name of the corresponding
  parameter.}
}
\description{
  Create a new JS function by binding values to extra
  parameters of an existing JS function.
}
\seealso{
  chmNewFunction
}

