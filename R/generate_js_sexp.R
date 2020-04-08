generate_js_sexp <- function(x, data, meta) {
  if (is.recursive(x)) {
    fn <- x[[1L]]
    args <- x[-1L]
    n <- length(args)
    values <- vcapply(args, generate_js_sexp, data, meta)

    if (fn == "(") {
      ret <- sprintf("(%s)", values[[1]])
    } else if (fn == "[") {
      pos <- js_array_access(args[[1L]], args[-1], data, meta)
      ret <- sprintf("%s[%s]", values[[1L]], pos)
    } else if (n == 2L && fn %in% odin:::FUNCTIONS_INFIX) {
      ret <- sprintf("%s %s %s", values[[1]], fn, values[[2]])
    } else if (fn == "if") {
      ## NOTE: The ternary operator has very low precendence, so I'm
      ## going to agressively parenthesise it.  This is strictly not
      ## needed when this expression is the only element of `expr` but
      ## that's hard to detect so we'll tolerate a few additional
      ## parens for now.
      ret <- sprintf("(%s ? %s : %s)",
                     values[[1L]], values[[2L]], values[[3L]])
    } else if (fn == "length") {
      ret <- generate_js_sexp(data$elements[[args[[1L]]]]$dimnames$length,
                              data, meta)
    } else if (fn == "dim") {
      dim <- data$elements[[args[[1L]]]]$dimnames$dim[[args[[2]]]]
      ret <- generate_js_sexp(dim, data, meta)
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
