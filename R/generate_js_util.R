js_flatten_eqs <- function(eqs) {
  unlist(unname(eqs))
}


js_function <- function(args, body, name = NULL) {
  if (is.null(name)) {
    start <- sprintf("function(%s) {", paste(args, collapse = ", "))
  } else {
    start <- sprintf("function %s(%s) {", name, paste(args, collapse = ", "))
  }
  if (length(body) > 0L) {
    body <- paste0("  ", body)
  }
  c(start, body, "}")
}


js_extract_variable <- function(x, data_elements, state, rewrite) {
  d <- data_elements[[x$name]]
  if (d$rank == 0L) {
    sprintf("%s[%s]", state, rewrite(x$offset))
  } else {
    sprintf("%s + %s", state, rewrite(x$offset))
  }
}


js_unpack_variable <- function(name, dat, state, rewrite) {
  x <- dat$data$variable$contents[[name]]
  rhs <- js_extract_variable(x, dat$data$elements, state, rewrite)
  sprintf_safe("var %s = %s;", x$name, rhs)
}


js_variable_reference <- function(x, data_info, state, rewrite) {
  sprintf("%s[%s]", state, rewrite(x$offset))
}
