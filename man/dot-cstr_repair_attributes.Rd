% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repair_attributes.R
\name{.cstr_repair_attributes}
\alias{.cstr_repair_attributes}
\title{Repair attributes after idiomatic construction}
\usage{
.cstr_repair_attributes(
  x,
  code,
  ...,
  ignore = NULL,
  idiomatic_class = NULL,
  remove = NULL,
  flag_s4 = TRUE,
  repair_names = FALSE
)
}
\arguments{
\item{x}{The object to construct}

\item{code}{The code constructing the object before attribute repair}

\item{...}{Forwarded to \code{.construct_apply()} when relevant}

\item{ignore}{The attributes that shouldn't be repaired, i.e. we expect them
to be set by the constructor already in \code{code}}

\item{idiomatic_class}{The class of the objects that the constructor produces,
if \code{x} is of class \code{idiomatic_class} there is no need to repair the class.}

\item{remove}{Attributes that should be removed, should rarely be useful.}

\item{flag_s4}{Boolean. Whether to use \code{asS4()} on the code of S4 objects,
set to \code{FALSE} when a constructor that produces S4 objects was used.}

\item{repair_names}{Boolean. Whether to repair the \code{names} attribute. Generally it is
generated by the constructor but it is needed for some corner cases}
}
\value{
A character vector
}
\description{
Exported for custom constructor design. In the general case an object might have more attributes than given by the idiomatic
construction. \code{.cstr_repair_attributes()} sets some of those attributes and ignores
others.
}
