## We're going to want to call these models from R a lot for testing,
## so let's expose them as full R objects:
odin_js_wrapper <- function(ir, options) {
  res <- generate_js(ir, options)
  context <- js_context(names(which(res$include)))
  context$eval(paste(res$code, collapse = "\n"))
  name <- res$name

  ret <- function(..., user = list(...), unused_user_action = NULL) {
    R6_odin_js_wrapper$new(context, name, user, res$features, res$ir,
                           unused_user_action)
  }
  attr(ret, "ir") <- ir
  class(ret) <- "odin_generator"
  ret
}


to_json_user <- function(user) {
  f <- function(x) {
    if (inherits(x, "JS_EVAL")) {
      class(x) <- "json"
    } else if (is.array(x)) {
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
  to_json(user, auto_unbox = TRUE, json_verbatim = TRUE)
}


##' @importFrom R6 R6Class
R6_odin_js_wrapper <- R6::R6Class(
  "odin_model",
  cloneable = FALSE,
  lock_objects = FALSE,

  private = list(
    context = NULL,
    name = NULL,
    features = NULL,
    internal_order = NULL,
    variable_order = NULL,
    output_order = NULL,
    ir_ = NULL,

    finalize = function() {
      private$js_eval(sprintf("delete %s;", private$name))
    },

    update_metadata = function() {
      private$internal_order <-
        private$context$get(sprintf("%s.metadata.internalOrder", private$name))
      private$variable_order <-
        private$context$get(sprintf("%s.metadata.variableOrder", private$name))
      private$output_order <-
        private$context$get(sprintf("%s.metadata.outputOrder", private$name))
    },

    js_call = function(...) {
      tryCatch(private$context$call(...), error = function(e) stop(e$message))
    },

    js_eval = function(...) {
      tryCatch(private$context$eval(...), error = function(e) stop(e$message))
    }
  ),

  public = list(
    initialize = function(context, generator, user, features, ir,
                          unused_user_action) {
      private$context <- context
      private$name <- sprintf("%s.%s", JS_INSTANCES, basename(tempfile("i")))
      private$features <- features

      ## For compatibility with odin, without having to write the full
      ## interface
      if (length(user) > 0L && !private$features$has_user) {
        tryCatch(do.call(function() NULL, user),
                 error = function(e) stop(e$message, call. = FALSE))
      }

      user_js <- to_json_user(user)
      unused_user_action <- unused_user_action %||%
        getOption("odin.unused_user_action", "warning")
      init <- sprintf("%s = new %s.%s(%s, %s);",
                      private$name, JS_GENERATORS, generator, user_js,
                      dquote(unused_user_action))
      if (features$discrete) {
        self$update <- self$rhs
      } else {
        self$deriv <- self$rhs
      }
      private$js_eval(init)
      private$update_metadata()

      private$ir_ <- ir
      lockEnvironment(self)
    },

    initial = function(t) {
      t_js <- to_json(scalar(t))
      private$js_call(sprintf("%s.initial", private$name), t_js)
    },

    ir = function() {
      private$ir_
    },

    set_user = function(..., user = list(...), unused_user_action = NULL) {
      unused_user_action <- unused_user_action %||%
        getOption("odin.unused_user_action", "warning")
      user_js <- to_json_user(user)
      private$js_call(sprintf("%s.setUser", private$name),
                      user_js, unused_user_action)
      private$update_metadata()
    },

    rhs = function(t, y) {
      ## TODO: check length of 'y' here?
      t_js <- to_json(scalar(t))
      y_js <- to_json(y, auto_unbox = FALSE)
      ret <- private$js_call(sprintf("%s.rhsEval", private$name),
                             t_js, y_js)
      ## This is super ugly but should do the trick for now:
      if (length(ret) > length(y)) {
        i <- seq_along(y)
        ret <- structure(ret[i], output = ret[-i])
      }
      ret
    },

    contents = function() {
      ret <- private$context$get(sprintf("%s.internal", private$name))
      order <- private$internal_order
      for (i in names(ret)) {
        d <- order[[i]]
        if (length(d) > 1) {
          dim(ret[[i]]) <- d
        }
      }
      ret
    },

    run = function(t, y = NULL, ..., tcrit = NULL, atol = NULL, rtol = NULL,
                   step_max_n = NULL, use_names = TRUE,
                   return_statistics = FALSE) {
      t_js <- to_json(t, auto_unbox = FALSE)
      if (is.null(y)) {
        y_js <- V8::JS("null")
      } else {
        y_js <- to_json(y, auto_unbox = FALSE)
      }
      if (is.null(tcrit)) {
        tcrit <- V8::JS("null")
      }
      if (is.null(atol)) {
        atol <- V8::JS("null")
      }
      if (is.null(rtol)) {
        rtol <- V8::JS("null")
      }
      if (is.null(step_max_n)) {
        step_max_n <- V8::JS("null")
      }

      ## NOTE: tcrit here is ignored when calling the discrete time
      ## model
      res <- private$js_call(sprintf("%s.run", private$name),
                             t_js, y_js, tcrit, atol, rtol, step_max_n)
      if (use_names) {
        colnames(res$y) <- res$names
      }

      if (return_statistics) {
        ## Convert into the same as for dde, which is a subset (we
        ## discard here lastError, stiffNNonstiff and stiffNStiff)
        statistics <- c(n_eval = res$statistics$nEval,
                        n_steps = res$statistics$nSteps,
                        n_accepted = res$statistics$nStepsAccepted,
                        n_rejected = res$statistics$nStepsRejected)
        attr(res$y, "statistics") <- statistics
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
