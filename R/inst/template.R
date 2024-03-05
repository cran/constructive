constructors$data.table <- new.env()

#' Constructive options for class CLASS
#'
#' These options will be used on objects of class 'data.table'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"CONSTRUCTOR"` (default): TODO.
#' * `"next"` : Use the constructor for the next supported class.
#' * `"TYPE"` : TYPE_DESC.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @return An object of class <constructive_options/constructive_options_CLASS>
#' @export
opts_data.table <- function(constructor = c("CONSTRUCTOR", "next", "TYPE"), ..., selfref = FALSE) {
  .cstr_combine_errors(
    {
      constructor <- .cstr_match_constructor(constructor, "CLASS")
    },
    ellipsis::check_dots_empty()
  )
  .cstr_options("CLASS", constructor = constructor, selfref = selfref)
}

#' @export
.cstr_construct.CLASS <- function(x, ...) {
  opts <- .cstr_fetch_opts("CLASS", ...)
  if (is_corrupted_CLASS(x) || opts$constructor == "next")
    return(NextMethod())
  if (is_corrupted_CLASS(x) || opts$constructor == "TYPE")
    return(.cstr_construct.TYPE(x, ...))
  constructor <- constructors$CLASS[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_CLASS <- function(x) {
  # TODO
  # check if x can't be constructed, assuming it has the right class, but not
  # the right structure
  FALSE
}

constructors$data.table$list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

constructors$data.table$data.table <- function(x, selfref, ...) {
  key <- attr(x, "sorted")
  if (!is.null(key)) {
    args <- c(x, key = key)
  } else {
    args <- x
  }
  code <- .cstr_apply(args, fun = "data.table::data.table", ...)
  repair_attributes_data.table(x, code, ..., selfref = selfref)
}

repair_attributes_CLASS <- function(x, code, ..., pipe = NULL, selfref = FALSE) {
  ignore <- c("row.names", "sorted")
  if (!selfref) ignore <- c(ignore, ".internal.selfref")
  if (identical(names(x), character())) ignore <- c(ignore, "names")
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = ignore,
    idiomatic_class = c("data.table", "data.frame")
  )
}
