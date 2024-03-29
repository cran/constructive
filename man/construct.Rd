% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construct.R
\name{construct}
\alias{construct}
\alias{construct_multi}
\title{Build code to recreate an object}
\usage{
construct(
  x,
  ...,
  data = NULL,
  pipe = NULL,
  check = NULL,
  compare = compare_options(),
  one_liner = FALSE,
  template = getOption("constructive_opts_template")
)

construct_multi(
  x,
  ...,
  data = NULL,
  pipe = NULL,
  check = NULL,
  compare = compare_options(),
  one_liner = FALSE,
  template = getOption("constructive_opts_template")
)
}
\arguments{
\item{x}{An object, for \code{construct_multi()} a named list or an environment.}

\item{...}{Constructive options built with the \verb{opts_*()} family of functions. See the "Constructive options"
section below.}

\item{data}{Named list or environment of objects we want to detect and mention by name (as opposed to
deparsing them further). Can also contain unnamed nested lists, environments, or
package names, in the latter case package exports and datasets will be considered.
In case of conflict, the last provided name is considered.}

\item{pipe}{Which pipe to use, either \code{"base"} or \code{"magrittr"}.
Defaults to \code{"base"} for R >= 4.2, otherwise to \code{"magrittr"}.}

\item{check}{Boolean. Whether to check if the created code reproduces the object
using \code{waldo::compare()}.}

\item{compare}{Parameters passed to \code{waldo::compare()}, built with \code{compare_options()}.}

\item{one_liner}{Boolean. Whether to collapse the output to a single line of code.}

\item{template}{A list of constructive options built with \verb{opts_*()} functions,
they will be overriden by \code{...}. Use it to set a default
behavior for \code{{constructive}}.}
}
\value{
An object of class 'constructive'.
}
\description{
\code{construct()} builds the code to reproduce one object, \code{construct_multi()}
builds the code to reproduce objects stored in a named list or environment.
}
\details{
\code{construct_multi()} recognizes promises, this means that for instance
\code{construct_multi(environment())} can be called in a
function and will construct unevaluated arguments using \code{delayedAssign()}.
Note however that \code{construct_multi(environment())} is equivalent to \code{construct_reprex()}
called without argument and the latter is preferred.
}
\examples{
construct(head(cars))
construct(head(cars), opts_data.frame("read.table"))
construct(head(cars), opts_data.frame("next"))
construct(iris$Species)
construct(iris$Species, opts_atomic(compress = FALSE), opts_factor("new_factor"))
}
\section{Constructive options}{
Constructive options provide a way to customize the output of `construct()`.
We can provide calls to `opts_*()` functions to the `...` argument. Each of these functions targets a specific type or class and is documented on its own page.

\itemize{
  \item \code{\link[=opts_array]{opts_array}(constructor = c("array", "next"), ...)}
  \item \code{\link[=opts_AsIs]{opts_AsIs}(constructor = c("I", "next", "atomic"), ...)}
  \item \code{\link[=opts_atomic]{opts_atomic}(..., trim = NULL, fill = c("default", "rlang", "+", "...", "none"), compress = TRUE, unicode_representation = c("ascii", "latin", "character", "unicode"), escape = FALSE)}
  \item \code{\link[=opts_classGeneratorFunction]{opts_classGeneratorFunction}(constructor = c("setClass"), ...)}
  \item \code{\link[=opts_classPrototypeDef]{opts_classPrototypeDef}(constructor = c("prototype"), ...)}
  \item \code{\link[=opts_classRepresentation]{opts_classRepresentation}(constructor = c("getClassDef"), ...)}
  \item \code{\link[=opts_constructive_options]{opts_constructive_options}(constructor = c("opts", "next"), ...)}
  \item \code{\link[=opts_data.frame]{opts_data.frame}(constructor = c("data.frame", "read.table", "next", "list"), ...)}
  \item \code{\link[=opts_data.table]{opts_data.table}(constructor = c("data.table", "next", "list"), ..., selfref = FALSE)}
  \item \code{\link[=opts_Date]{opts_Date}(constructor = c("as.Date", "as_date", "date", "new_date", "as.Date.numeric", "as_date.numeric", "next", "atomic"), ..., origin = "1970-01-01")}
  \item \code{\link[=opts_dm]{opts_dm}(constructor = c("dm", "next", "list"), ...)}
  \item \code{\link[=opts_dots]{opts_dots}(constructor = c("default"), ...)}
  \item \code{\link[=opts_environment]{opts_environment}(constructor = c(".env", "list2env", "as.environment", "new.env", "topenv", "new_environment"), ..., recurse = FALSE, predefine = FALSE)}
  \item \code{\link[=opts_externalptr]{opts_externalptr}(constructor = c("default"), ...)}
  \item \code{\link[=opts_factor]{opts_factor}(constructor = c("factor", "as_factor", "new_factor", "next", "atomic"), ...)}
  \item \code{\link[=opts_formula]{opts_formula}(constructor = c("~", "formula", "as.formula", "new_formula", "next"), ..., environment = TRUE)}
  \item \code{\link[=opts_function]{opts_function}(constructor = c("function", "as.function", "new_function"), ..., environment = TRUE, srcref = FALSE, trim = NULL)}
  \item \code{\link[=opts_grouped_df]{opts_grouped_df}(constructor = c("default", "next", "list"), ...)}
  \item \code{\link[=opts_language]{opts_language}(constructor = c("default"), ...)}
  \item \code{\link[=opts_Layer]{opts_Layer}(constructor = c("default", "layer", "environment"), ...)}
  \item \code{\link[=opts_list]{opts_list}(constructor = c("list", "list2"), ..., trim = NULL, fill = c("vector", "new_list", "+", "...", "none"))}
  \item \code{\link[=opts_matrix]{opts_matrix}(constructor = c("matrix", "array", "next", "atomic"), ...)}
  \item \code{\link[=opts_mts]{opts_mts}(constructor = c("ts", "next", "atomic"), ...)}
  \item \code{\link[=opts_numeric_version]{opts_numeric_version}(constructor = c("numeric_version", "next", "atomic"), ...)}
  \item \code{\link[=opts_ordered]{opts_ordered}(constructor = c("ordered", "factor", "new_ordered", "next", "atomic"), ...)}
  \item \code{\link[=opts_package_version]{opts_package_version}(constructor = c("package_version", "next", "atomic"), ...)}
  \item \code{\link[=opts_pairlist]{opts_pairlist}(constructor = c("pairlist", "pairlist2"), ...)}
  \item \code{\link[=opts_POSIXct]{opts_POSIXct}(constructor = c("as.POSIXct", ".POSIXct", "as_datetime", "as.POSIXct.numeric", "as_datetime.numeric", "next", "atomic"), ..., origin = "1970-01-01")}
  \item \code{\link[=opts_POSIXlt]{opts_POSIXlt}(constructor = c("as.POSIXlt", "next", "list"), ...)}
  \item \code{\link[=opts_quosure]{opts_quosure}(constructor = c("new_quosure", "next", "language"), ...)}
  \item \code{\link[=opts_quosures]{opts_quosures}(constructor = c("new_quosures", "next", "list"), ...)}
  \item \code{\link[=opts_R_system_version]{opts_R_system_version}(constructor = c("R_system_version", "next", "atomic"), ...)}
  \item \code{\link[=opts_rowwise_df]{opts_rowwise_df}(constructor = c("default", "next", "list"), ...)}
  \item \code{\link[=opts_S4]{opts_S4}(constructor = c("new"), ...)}
  \item \code{\link[=opts_tbl_df]{opts_tbl_df}(constructor = c("tibble", "tribble", "next", "list"), ..., trailing_comma = TRUE)}
  \item \code{\link[=opts_ts]{opts_ts}(constructor = c("ts", "next", "atomic"), ...)}
  \item \code{\link[=opts_vctrs_list_of]{opts_vctrs_list_of}(constructor = c("list_of", "list"), ...)}
  \item \code{\link[=opts_weakref]{opts_weakref}(constructor = c("new_weakref"), ...)}
}
}

