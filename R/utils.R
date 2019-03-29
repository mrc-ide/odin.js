vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


to_json <- function(x, auto_unbox = FALSE) {
  V8::JS(jsonlite::toJSON(x, auto_unbox = auto_unbox, digits = NA))
}


scalar <- function(x) {
  jsonlite::unbox(x)
}
