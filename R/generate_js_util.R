js_flatten_eqs <- function(eqs) {
  unlist(unname(eqs))
}


js_function <- function(args, body, name = NULL) {
  if (is.null(name)) {
    start <- sprintf("function(%s) {", paste(args, collapse = ", "))
  } else {
    start <- sprintf("function %s(%s) {", name, paste(args, collapse = ", "))
  }
  c(start, paste0("  ", body), "}")
}


js_unpack_variable <- function(name, dat, state, rewrite) {
  browser()
}


js_variable_reference <- function(x, data_info, state, rewrite) {
  sprintf("%s[%s]", state, rewrite(x$offset))
}
