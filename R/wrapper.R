## We're going to want to call these models from R a lot for testing,
## so let's expose them as full R objects:
odin_js_wrapper <- function(ir, options) {
  res <- generate_js(ir, options)
  context <- js_context(names(which(res$include)))
  context$eval(paste(res$code, collapse = "\n"))
  name <- res$name

  ret <- function(..., user = list(...)) {
    R6_odin_js_wrapper$new(context, name, user, res$discrete)
  }
  attr(ret, "ir") <- ir
  class(ret) <- "odin_generator"
  ret
}


to_json_user <- function(user) {
  f <- function(x) {
    if (is.array(x)) {
      x <- list(data = c(x), dim = I(dim(x)))
    } else if (length(x) > 1L || inherits(x, "AsIs")) {
      x <- list(data = x, dim = I(length(x)))
    }
    x
  }
  if (length(user) > 0) {
    stopifnot(!is.null(names(user)))
  }
  user <- lapply(user, f)
  to_json(user, auto_unbox = TRUE)
}


##' @importFrom R6 R6Class
R6_odin_js_wrapper <- R6::R6Class(
  "odin_model",
  cloneable = FALSE,
  lock_objects = FALSE,

  private = list(
    context = NULL,
    name = NULL,
    variable_order = NULL,
    output_order = NULL,

    finalize = function() {
      private$context$eval(sprintf("delete %s;", private$name))
    },

    update_metadata = function() {
      private$variable_order <-
        private$context$get(sprintf("%s.metadata.variableOrder", private$name))
      private$output_order <-
        private$context$get(sprintf("%s.metadata.outputOrder", private$name))
    }
  ),

  public = list(
    initialize = function(context, generator, user, discrete) {
      private$context <- context
      private$name <- sprintf("%s.%s", JS_INSTANCES, basename(tempfile("i")))
      user_js <- to_json_user(user)
      init <- sprintf("%s = new %s.%s(%s);",
                      private$name, JS_GENERATORS, generator, user_js)
      if (discrete) {
        self$update <- self$rhs
      } else {
        self$deriv <- self$rhs
      }
      private$context$eval(init)
      private$update_metadata()
      lockEnvironment(self)
    },

    initial = function(t) {
      t_js <- to_json(scalar(t))
      private$context$call(sprintf("%s.initial", private$name), t_js)
    },

    set_user = function(user) {
      user_js <- to_json_user(user)
      private$context$call(sprintf("%s.setUser", private$name), user_js)
      private$update_metadata()
    },

    rhs = function(t, y) {
      ## TODO: check length of 'y' here?
      t_js <- to_json(scalar(t))
      y_js <- to_json(y, auto_unbox = FALSE)
      ret <- private$context$call(sprintf("%s.rhsEval", private$name),
                                  t_js, y_js)
      ## This is super ugly but should do the trick for now:
      if (length(ret) > length(y)) {
        i <- seq_along(y)
        ret <- structure(ret[i], output = ret[-i])
      }
      ret
    },

    contents = function() {
      private$context$get(sprintf("%s.internal", private$name))
    },

    run = function(t, y = NULL, ..., tcrit = NULL, use_names = TRUE) {
      t_js <- to_json(t, auto_unbox = FALSE)
      if (is.null(y)) {
        y_js <- V8::JS("null")
      } else {
        y_js <- to_json(y, auto_unbox = FALSE)
      }
      if (is.null(tcrit)) {
        tcrit <- V8::JS("null")
      }

      ## NOTE: tcrit here is ignored when calling the discrete time
      ## model
      res <- private$context$call(sprintf("%s.run", private$name),
                                  t_js, y_js, tcrit)
      if (use_names) {
        colnames(res$y) <- res$names
      }
      res$y
    },

    transform_variables = function(y) {
      odin:::support_transform_variables(y, private)
    }
  ))


##' @importFrom V8 v8
js_context <- function(include) {
  ct <- V8::v8()
  js_file <- function(path) {
    system.file(path, package = "odin.js", mustWork = TRUE)
  }

  ct$source(js_file("dopri.js"))
  ct$source(js_file("support.js"))
  for (f in include) {
    ct$source(js_file(f))
  }

  ct$eval(sprintf("var %s = {};", JS_GENERATORS))
  ct$eval(sprintf("var %s = {};", JS_INSTANCES))
  ct
}
