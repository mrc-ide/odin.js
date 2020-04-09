generate_js_equations <- function(dat, rewrite) {
  lapply(dat$equations, generate_js_equation, dat, rewrite)
}


generate_js_equation <- function(eq, dat, rewrite) {
  f <- switch(
    eq$type,
    expression_scalar = generate_js_equation_scalar,
    expression_array = generate_js_equation_array,
    alloc = generate_js_equation_alloc,
    copy = generate_js_equation_copy,
    user = generate_js_equation_user,
    stop(sprintf("Unknown type '%s' [odin.js bug]")))

  data_info <- dat$data$elements[[eq$lhs]]
  stopifnot(!is.null(data_info))

  f(eq, data_info, dat, rewrite)
}


generate_js_equation_scalar <- function(eq, data_info, dat, rewrite) {
  location <- data_info$location

  if (location == "transient") {
    lhs <- sprintf("var %s", eq$lhs)
  } else if (location == "internal") {
    lhs <- rewrite(eq$lhs)
  } else {
    offset <- dat$data[[location]]$contents[[data_info$name]]$offset
    storage <- if (location == "variable") dat$meta$result else dat$meta$output
    lhs <- sprintf("%s[%s]", storage, rewrite(offset))
  }

  rhs <- rewrite(eq$rhs$value)
  sprintf("%s = %s;", lhs, rhs)
}


generate_js_equation_copy <- function(eq, data_info, dat, rewrite) {
  x <- dat$data$output$contents[[data_info$name]]
  if (data_info$rank == 0L) {
    sprintf("output[%s] = %s", rewrite(x$offset), rewrite(eq$lhs))
  } else {
    c(sprintf("for (var i = 0; i < %s; ++i) {",
              rewrite(data_info$dimnames$length)),
      sprintf("  output[%s + i] = %s[i]", rewrite(x$offset), rewrite(eq$lhs)),
      "}")
  }
}


generate_js_equation_user <- function(eq, data_info, dat, rewrite) {
  user <- dat$meta$user
  internal <- dat$meta$internal

  rank <- data_info$rank
  is_integer <- if (data_info$storage_type == "int") "true" else "false"
  min <- rewrite(eq$user$min %||% "null")
  max <- rewrite(eq$user$max %||% "null")
  default <- rewrite(eq$user$default) %||% "null"

  if (eq$user$dim) {
    len <- data_info$dimnames$length
    ret <- c(
      sprintf("var %s = new Array(%d);", len, rank + 1),
      sprintf(
        'getUserArrayDim(%s, "%s", %s, %s, %s, %s, %s, %s);',
        user, eq$lhs, internal, len, default,
        min, max, is_integer),
      sprintf("%s = %s[%d];", rewrite(len), len, 0),
      sprintf("%s = %s[%d];",
              vcapply(data_info$dimnames$dim, rewrite), len,
              seq_len(rank)))
  } else {
    if (rank == 0L) {
      size <- "null"
      ret <- sprintf(
        'getUser(%s, "%s", %s, %s, %s, %s, %s, %s);',
        user, eq$lhs, internal, size, default, min, max, is_integer)
    } else {
      if (rank == 1L) {
        dim <- rewrite(data_info$dimnames$length)
        size <- sprintf("[%s, %s]", dim, dim)
      } else {
        dim <- vcapply(c(data_info$dimnames$length, data_info$dimnames$dim),
                       rewrite)
        size <- sprintf("[%s]", paste(dim, collapse = ", "))
      }
      ret <- sprintf(
        'getUserArray(%s, "%s", %s, %s, %s, %s, %s, %s);',
        user, eq$lhs, internal, size, default,
        min, max, is_integer)
    }
  }
  ret
}


generate_js_equation_array <- function(eq, data_info, dat, rewrite) {
  lhs <- generate_js_equation_array_lhs(eq, data_info, dat, rewrite)
  lapply(eq$rhs, function(x)
    generate_js_equation_array_rhs(x$value, x$index, lhs, rewrite))
}


generate_js_equation_alloc <- function(eq, data_info, dat, rewrite) {
  lhs <- rewrite(eq$lhs)
  len <- rewrite(data_info$dimnames$length)
  sprintf("%s = new Array(%s);", lhs, len)
}


generate_js_equation_array_lhs <- function(eq, data_info, dat, rewrite) {
  if (eq$type == "expression_array") {
    index <- vcapply(eq$rhs[[1]]$index, "[[", "index")
  } else {
    ## This is here to support delays, which are not yet supported.
    ## This *shoul* work but leaving in an assertion so that I
    ## remember to double check it and remove the "no coverage"
    ## marker.
    stop("check for delays") # nocov
    index <- lapply(eq$rhs$index, "[[", "index") # nocov
  }
  location <- data_info$location

  f <- function(i) {
    if (i == 1) {
      sprintf("%s - 1", index[[i]])
    } else {
      sprintf("%s * (%s - 1)",
              rewrite(data_info$dimnames$mult[[i]]), index[[i]])
    }
  }

  pos <- paste(vcapply(seq_along(index), f), collapse = " + ")
  if (location == "internal") {
    lhs <- sprintf("%s[%s]", rewrite(data_info$name), pos)
  } else {
    offset <- rewrite(dat$data[[location]]$contents[[data_info$name]]$offset)
    storage <- if (location == "variable") dat$meta$result else dat$meta$output
    lhs <- sprintf("%s[%s + %s]", storage, offset, pos)
  }

  lhs
}


generate_js_equation_array_rhs <- function(value, index, lhs, rewrite) {
  ret <- sprintf("%s = %s;", lhs, rewrite(value))
  seen_range <- FALSE
  for (idx in rev(index)) {
    if (idx$is_range) {
      seen_range <- TRUE
      loop <- sprintf("for (var %s = %s; %s <= %s; ++%s) {",
                      idx$index, rewrite(idx$value[[2]]),
                      idx$index, rewrite(idx$value[[3]]),
                      idx$index)
      ret <- c(loop, paste0("  ", ret), "}")
    } else {
      ret <- c(sprintf("var %s = %s;", idx$index, rewrite(idx$value)),
               ret)
    }
  }
  if (!seen_range || !index[[1]]$is_range) {
    ret <- c("{", paste("  ", ret), "}")
  }
  ret
}
