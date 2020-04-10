generate_js <- function(ir, options) {
  dat <- odin::odin_ir_deserialise(ir)

  rewrite <- function(x) {
    generate_js_sexp(x, dat$data, dat$meta)
  }

  features <- vlapply(dat$features, identity)
  supported <- c("initial_time_dependent", "has_array", "has_user")
  unsupported <- setdiff(names(features)[features], supported)
  if (length(unsupported) > 0L) {
    stop("Using unsupported features: ",
         paste(squote(unsupported), collapse = ", "))
  }

  eqs <- generate_js_equations(dat, rewrite)
  core <- generate_js_core(eqs, dat, rewrite)

  ## This is all we need to dump out
  list(code = generate_js_generator(core, dat),
       name = dat$config$base)
}


generate_js_core <- function(eqs, dat, rewrite) {
  list(create = generate_js_core_create(eqs, dat, rewrite),
       set_user = generate_js_core_set_user(eqs, dat, rewrite),
       rhs = generate_js_core_deriv(eqs, dat, rewrite),
       rhs_eval = generate_js_core_rhs_eval(eqs, dat, rewrite),
       run = generate_js_core_run(eqs, dat, rewrite),
       metadata = generate_js_core_metadata(eqs, dat, rewrite),
       coef = generate_js_coef(eqs, dat, rewrite),
       initial_conditions = generate_js_core_initial_conditions(
         eqs, dat, rewrite))
}


generate_js_core_create <- function(eqs, dat, rewrite) {
  body <- odin:::collector()
  body$add("this.%s = {};", dat$meta$internal)
  body$add("var %s = this.%s;", dat$meta$internal, dat$meta$internal)
  body$add(js_flatten_eqs(eqs[dat$components$create$equations]))
  body$add("this.setUser(%s);", dat$meta$user)
  args <- dat$meta$user
  js_function(args, body$get(), dat$config$base)
}


generate_js_core_set_user <- function(eqs, dat, rewrite) {
  update_metadata <- "this.updateMetadata();"
  if (dat$features$has_user) {
    body <- c(
      sprintf("var %s = this.%s;", dat$meta$internal, dat$meta$internal),
      js_flatten_eqs(eqs[dat$components$user$equations]),
      update_metadata)
  } else {
    body <- update_metadata
  }
  args <- dat$meta$user
  js_function(args, body)
}


generate_js_core_deriv <- function(eqs, dat, rewrite) {
  variables <- dat$components$rhs$variables
  equations <- dat$components$rhs$equations

  internal <- sprintf("var %s = this.%s;", dat$meta$internal, dat$meta$internal)
  unpack <- lapply(variables, js_unpack_variable, dat, dat$meta$state, rewrite)

  body <- js_flatten_eqs(c(internal, unpack, eqs[equations]))

  args <- c(dat$meta$time, dat$meta$state, dat$meta$result)
  js_function(args, body)
}


generate_js_core_run <- function(eqs, dat, rewrite) {
  args <- c("times", "y0")
  body <- "return integrateOdin(this, times, y0);"
  js_function(args, body)
}


generate_js_coef <- function(eqs, dat, rewrite) {
  if (!dat$features$has_user) {
    return("{}")
  }
  js_dict <- function(x) {
    sprintf("{%s}", paste(sprintf("%s: %s", names(x), x), collapse = ", "))
  }
  f <- function(x) {
    data_info <- dat$data$elements[[x$name]]
    if (is.null(x$user$default)) {
      default <- "null"
    } else {
      default <- as.character(x$user$default)
    }
    d <- c(has_default = tolower(is.null(x$user$default)),
           default = default,
           rank = as.character(data_info$rank),
           min = as.character(x$user$min %||% "-Infinity"),
           max = as.character(x$user$max %||% "Infinity"),
           integer = tolower(data_info$storage_type == "integer"))
    js_dict(d)
  }
  js_dict(vcapply(dat$equations[names(dat$user)], f))
}


generate_js_core_metadata <- function(eqs, dat, rewrite) {
  stopifnot(!dat$features$has_output)

  body <- c("this.metadata = {};",
            "var internal = this.internal;")
  if (dat$features$has_array) {
    contents <- dat$data$elements[names(dat$data[["variable"]]$contents)]
    is_scalar <- vlapply(contents, function(x) x$rank == 0)
    ynames_scalar <- c("t", names(is_scalar)[is_scalar])
    ynames <- sprintf("this.metadata.ynames = [%s];",
                      paste(dquote(ynames_scalar), collapse = ", "))
    for (el in contents[!is_scalar]) {
      if (el$rank == 1L) {
        len <- rewrite(el$dimnames$length)
        ynames <- c(
          ynames,
          sprintf("for (var i = 1; i <= %s; ++i) {", len),
          sprintf('  this.metadata.ynames.push("%s[" + i + "]");', el$name),
          sprintf("}"))
      } else {
        rank <- el$rank
        index <- paste0("i", seq_len(rank))
        pos <- paste(index, collapse = ' + "," + ')
        ynames1 <- sprintf('this.metadata.ynames.push("%s[" + %s + "]");',
                           el$name, pos)
        for (i in seq_len(rank)) {
          len <- rewrite(el$dimnames$dim[[i]])
          loop <- sprintf("for (var %s = 1; %s <= %s; ++%s) {",
                          index[[i]], index[[i]], len, index[[i]])
          ynames1 <- c(loop, paste0("  ", ynames1), "}")
        }

        ynames <- c(ynames, ynames1)
      }
    }
    body <- c(body, ynames)
  } else {
    ynames <- c(dat$meta$time, names(dat$data$variable$contents))
    body <- c(body,
              sprintf("this.metadata.ynames = [%s];",
                      paste(dquote(ynames), collapse = ", ")))
  }
  js_function(NULL, body)
}


generate_js_core_rhs_eval <- function(eqs, dat, rewrite) {
  args <- c(dat$meta$time, dat$meta$state)
  body <- c(
    sprintf("var %s = zeros(%s.length);", dat$meta$result, dat$meta$state),
    sprintf("this.rhs(%s, %s, %s);",
            dat$meta$time, dat$meta$state, dat$meta$result),
    sprintf("return %s;", dat$meta$result))
  js_function(args, body)
}


generate_js_core_initial_conditions <- function(eqs, dat, rewrite) {
  set_initial <- function(el) {
    data_info <- dat$data$elements[[el$name]]
    if (data_info$rank == 0L) {
      lhs <- sprintf("%s[%s]", dat$meta$state, rewrite(el$offset))
      sprintf("%s = %s.%s;", lhs, dat$meta$internal, el$initial)
    } else {
      c(sprintf("for (var i = 0; i < %s; ++i) {",
                rewrite(data_info$dimnames$length)),
        sprintf("  %s[%s + i] = %s.%s[i];",
                dat$meta$state, rewrite(el$offset),
                dat$meta$internal, el$initial),
        "}")
    }
  }

  internal <- sprintf("var %s = this.%s;",
                      dat$meta$internal, dat$meta$internal)
  if (length(dat$components$initial$equations) == 0) {
    eqs_initial <- NULL
  } else {
    subs <- lapply(dat$data$variable$contents, function(x) rewrite(x$initial))
    eqs_initial <- dat$equations[dat$components$initial$equations]
    eqs_initial <- lapply(odin:::ir_substitute(eqs_initial, subs),
                          generate_js_equation, dat, rewrite)
  }

  initial <- js_flatten_eqs(lapply(dat$data$variable$contents, set_initial))

  body <- odin:::collector()
  body$add(internal)
  body$add(js_flatten_eqs(eqs_initial))
  body$add("var %s = zeros(%s);",
           dat$meta$state, rewrite(dat$data$variable$length))
  body$add(initial)
  body$add("return %s;", dat$meta$state)

  args <- dat$meta$time
  js_function(args, body$get())
}


generate_js_generator <- function(core, dat) {
  base <- dat$config$base
  method <- function(name, x) {
    n <- length(x)
    x[[1]] <- sprintf("%s.prototype.%s = %s", base, name, x[[1]])
    x[[n]] <- paste0(x[[n]], ";")
    x
  }
  field <- function(name, x) {
    stopifnot(length(x) == 1)
    sprintf("%s.prototype.%s = %s;", base, name, x)
  }

  body <- odin:::collector()
  body$add(core$create)
  body$add(method("setUser", core$set_user))
  body$add(method("rhs", core$rhs))
  body$add(method("rhsEval", core$rhs_eval))
  body$add(method("initial", core$initial_conditions))
  body$add(method("run", core$run))
  body$add(field("coef", core$coef))
  body$add(method("updateMetadata", core$metadata))
  body$add("return %s;", base)

  c(sprintf("%s.%s = (function() {", JS_GENERATORS, base),
    paste0("  ", body$get()),
    "}());")
}
