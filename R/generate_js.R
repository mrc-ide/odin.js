generate_js <- function(dat, options, context) {
  res <- generate_js_code(dat, options)

  context$eval(paste(res, collapse = "\n"))
  odin_js_wrapper(context, dat$config$base)
}


generate_js_code <- function(dat, options) {
  rewrite <- function(x) {
    generate_js_sexp(x, dat$data, dat$meta)
  }

  eqs <- generate_js_equations(dat, rewrite)
  core <- generate_js_core(eqs, dat, rewrite)

  ## This is all we need to dump out
  generate_js_generator(core, dat)
}


generate_js_core <- function(eqs, dat, rewrite) {
  list(create = generate_js_core_create(eqs, dat, rewrite),
       rhs = generate_js_core_deriv(eqs, dat, rewrite),
       rhs_eval = generate_js_core_rhs_eval(eqs, dat, rewrite),
       run = generate_js_core_run(eqs, dat, rewrite),
       initial_conditions = generate_js_core_initial_conditions(
         eqs, dat, rewrite))
}


generate_js_core_create <- function(eqs, dat, rewrite) {
  body <- odin:::collector()
  body$add("this.%s = {};", dat$meta$internal)
  body$add("var %s = this.%s;", dat$meta$internal, dat$meta$internal)
  body$add(js_flatten_eqs(eqs[dat$components$create$equations]))
  js_function(NULL, body$get(), dat$config$base)
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
  body <- "return integrate_odin(this, times, y0);"
  js_function(args, body)
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
    lhs <- js_variable_reference(el, data_info, dat$meta$state, rewrite)
    sprintf("%s = %s.%s;", lhs, dat$meta$internal, el$initial)
  }

  internal <- sprintf("var %s = this.%s;",
                      dat$meta$internal, dat$meta$internal)
  if (length(dat$components$initial$equations) == 0) {
    eqs_initial <- NULL
  } else {
    browser()
  }

  initial <- js_flatten_eqs(lapply(dat$data$variable$contents, set_initial))
  if (dat$features$initial_time_dependent) {
    browser()
  }

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
    x[[1]] <- sprintf("%s.prototype.%s = %s", base, name, x[[1]])
    x
  }

  body <- odin:::collector()
  body$add(core$create)
  body$add(method("rhs", core$rhs))
  body$add(method("rhs_eval", core$rhs_eval))
  body$add(method("initial", core$initial_conditions))
  body$add(method("run", core$run))
  ## body$add(prefix("this.prototype.setUser = ", core$set_user))
  body$add("return %s;", base)

  c(sprintf("%s.%s = (function() {", JS_GENERATORS, base),
    paste0("  ", body$get()),
    "}());")
}
