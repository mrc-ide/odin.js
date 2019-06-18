generate_js_sexp <- function(x, data, meta) {
  if (is.recursive(x)) {
    fn <- x[[1L]]
    args <- x[-1L]
    n <- length(args)
    values <- vcapply(args, generate_js_sexp, data, meta)

    if (fn == "(") {
      ret <- sprintf("(%s)", values[[1]])
    } else if (n == 2L && fn %in% odin:::FUNCTIONS_INFIX) {
      ret <- sprintf("%s %s %s", values[[1]], fn, values[[2]])
    } else {
      if (any(FUNCTIONS_MATH == fn)) {
        fn <- sprintf("Math.%s", fn)
      }
      ## if (!any(names(FUNCTIONS) == fn)) {
      ##   stop(sprintf("unsupported function '%s' [odin bug]", fn)) # nocov
      ## }
      ret <- sprintf("%s(%s)", fn, paste(values, collapse = ", "))
    }
    ret
  } else if (is.character(x)) {
    location <- data$elements[[x]]$location
    if (!is.null(location) && location == "internal") {
      sprintf("%s.%s", meta$internal, x)
    } else {
      x
    }
  } else if (is.numeric(x)) {
    deparse(x, control = "digits17")
  }
}


FUNCTIONS_RENAME <- c(
  "^" = "Math.pow",
  ceiling = "Math.ceil"
)


FUNCTIONS_MATH <- c(
  "sqrt",
  "exp", "expm1", "log", "log2", "log10", "log1p",
  "cos", "sin", "tan",
  "acos", "asin", "atan", "atan2",
  "cosh", "sinh", "tanh",
  "acosh", "asinh", "atanh",
  "abs", "floor", "round", "trunc")