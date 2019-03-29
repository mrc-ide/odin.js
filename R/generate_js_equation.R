generate_js_equations <- function(dat, rewrite) {
  lapply(dat$equations, generate_js_equation, dat, rewrite)
}


generate_js_equation <- function(eq, dat, rewrite) {
  f <- switch(
    eq$type,
    expression_scalar = generate_js_equation_scalar,
    user = generate_js_equation_user,
    stop("Unknown type"))

  data_info <- dat$data$elements[[eq$lhs]]
  stopifnot(!is.null(data_info))

  f(eq, data_info, dat, rewrite)
}


generate_js_equation_scalar <- function(eq, data_info, dat, rewrite) {
  location <- data_info$location

  if (location == "transient") {
    lhs <- sprintf("const %s", eq$lhs)
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


generate_js_equation_user <- function(eq, data_info, dat, rewrite) {
  user <- dat$meta$user
  internal <- dat$meta$internal

  rank <- data_info$rank
  is_integer <- if (data_info$storage_type == "int") "true" else "false"
  min <- rewrite(eq$user$min %||% "null")
  max <- rewrite(eq$user$max %||% "null")

  if (eq$user$dim) {
    stop("not implemented")
  } else {
    lhs <- rewrite(eq$lhs)
    storage_type <- data_info$storage_type
    default <- rewrite(eq$user$default) %||% "null"
    if (rank == 0L) {
      size <- "null"
    } else {
      stop("not implemented")
    }
    sprintf_safe(
      'get_user(%s, "%s", %s, %s, %s, %s, %s, %s);',
      user, eq$lhs, internal, size, default, min, max, is_integer)
  }
}
