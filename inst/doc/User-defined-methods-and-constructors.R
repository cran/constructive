## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(constructive)

## -----------------------------------------------------------------------------
.cstr_construct
.cstr_construct(letters)
construct(letters)

## -----------------------------------------------------------------------------
.cstr_construct.Date <- function(x, ...) {
  opts <- .cstr_fetch_opts("Date", ...)
  if (is_corrupted_Date(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$Date[[opts$constructor]]
  constructor(x, ..., origin = opts$origin)
}

## ---- eval=FALSE--------------------------------------------------------------
#  opts_Date <- function(
#      constructor = c(
#        "as.Date", "as_date", "date", "new_date",  "as.Date.numeric",
#        "as_date.numeric", "next", "atomic"
#      ),
#      ...,
#      origin = "1970-01-01"
#    ) {
#    .cstr_combine_errors(
#      constructor <- .cstr_match_constructor(constructor),
#      ellipsis::check_dots_empty()
#    )
#    .cstr_options("Date", constructor = constructor, origin = origin)
#  }

## -----------------------------------------------------------------------------
# .cstr_fetch_opts() takes a class and the dots and retrieves the relevant options
# if none were provided it falls back on the default value for the relevant opts_?() function
test <- function(...) {
  .cstr_fetch_opts("Date", ...)
}
test(opts_Date("as_date"), opts_data.frame("read.table"))
test()

## ---- error = TRUE------------------------------------------------------------
x <- structure("12345", class = "Date")
x
x + 1

## -----------------------------------------------------------------------------
is_corrupted_Date <- function(x) {
  !is.double(x)
}

## -----------------------------------------------------------------------------
construct(x)

## ---- eval = FALSE------------------------------------------------------------
#  constructor <- constructors$Date[[opts$constructor]]

## -----------------------------------------------------------------------------
constructors$Date$as.Date

## -----------------------------------------------------------------------------
x <- structure(c(12345, 20000), class = "Date")
y <- structure(c(12345, Inf), class = "Date")
constructors$Date$as.Date(x)
constructors$Date$as.Date(y)

## -----------------------------------------------------------------------------
x <- structure(c(12345, 20000), class = "Date", some_attr = 42)
# attributes are not visible due to "Date"'s printing method
x

# but constructive retrieves them
constructors$Date$as.Date(x)

## -----------------------------------------------------------------------------
constructive:::repair_attributes_Date

constructive:::repair_attributes_factor

## ---- eval = FALSE------------------------------------------------------------
#  # in zzz.R
#  .onLoad <- function(libname, pkgname) {
#    .cstr_register_constructors(
#      class_name,
#      constructor_name1 = constructor1,
#      constructor_name2 = constructor2
#    )
#  }

