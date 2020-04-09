## We're going to want to call these models from R a lot for testing,
## so let's expose them as full R objects:
odin_js_wrapper <- function(ir, options) {
  context <- js_context()

  res <- generate_js(ir, options)
  context$eval(paste(res$code, collapse = "\n"))
  name <- res$name

  ret <- function(..., user = list(...)) {
    R6_odin_js_wrapper$new(context, name, user)
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
  private = list(
    context = NULL,
    name = NULL,

    finalize = function() {
      private$context$eval(sprintf("delete %s;", private$name))
    }
  ),

  public = list(
    initialize = function(context, generator, user) {
      private$context <- context
      private$name <- sprintf("%s.%s", JS_INSTANCES, basename(tempfile("i")))
      user_js <- to_json_user(user)
      init <- sprintf("%s = new %s.%s(%s);",
                      private$name, JS_GENERATORS, generator, user_js)
      private$context$eval(init)
    },

    initial = function(t) {
      t_js <- to_json(scalar(t))
      private$context$call(sprintf("%s.initial", private$name), t_js)
    },

    set_user = function(user) {
      user_js <- to_json_user(user)
      private$context$call(sprintf("%s.setUser", private$name), user_js)
    },

    deriv = function(t, y) {
      ## TODO: check length of 'y' here?
      t_js <- to_json(scalar(t))
      y_js <- to_json(y, auto_unbox = FALSE)
      private$context$call(sprintf("%s.rhsEval", private$name), t_js, y_js)
    },

    contents = function() {
      private$context$get(sprintf("%s.internal", private$name))
    },

    run = function(t, y = NULL, ..., use_names = TRUE) {
      t_js <- to_json(t, auto_unbox = FALSE)
      if (is.null(y)) {
        y_js <- V8::JS("null")
      } else {
        y_js <- to_json(y, auto_unbox = FALSE)
      }
      res <- private$context$call(sprintf("%s.run", private$name), t_js, y_js)
      if (use_names) {
        colnames(res$y) <- res$names
      }
      res$y
    }
  ))


##' @importFrom V8 v8
js_context <- function() {
  ct <- V8::v8()
  ct$source(system.file("dopri.js", package = "odin.js", mustWork = TRUE))
  ct$source(system.file("support.js", package = "odin.js", mustWork = TRUE))
  ct$eval(sprintf("var %s = {};", JS_GENERATORS))
  ct$eval(sprintf("var %s = {};", JS_INSTANCES))
  ct
}
