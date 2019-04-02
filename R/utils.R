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


squote <- function(x) {
  sprintf("'%s'", x)
}


dquote <- function(x) {
  sprintf('"%s"', x)
}


package_js <- function(name) {
  readLines(system.file(name, package = "odin.js", mustWork = TRUE),
            warn = FALSE)
}


js_null <- function() {
  V8::JS("null")
}
