vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}


viapply <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
}


vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}


vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


sprintf_safe <- function(fmt, ...) {
  dots <- list(...)
  if (any(vlapply(dots, is.null))) {
    stop("Passed empty format parameter to formatter")
  }
  if (length(dots) == 0) {
    fmt
  } else {
    sprintf(fmt, ...)
  }
}


to_json <- function(x, auto_unbox = FALSE) {
  V8::JS(jsonlite::toJSON(x, auto_unbox = auto_unbox, digits = NA))
}


scalar <- function(x) {
  jsonlite::unbox(x)
}
