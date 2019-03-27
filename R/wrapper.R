## We're going to want to call these models from R a lot for testing,
## so let's expose them as full R objects:
odin_js_wrapper <- function(context, name, meta) {
  force(context)
  force(name)
  function() {
    R6_odin_js_wrapper$new(context, name)
  }
}


R6_odin_js_wrapper <- R6::R6Class(
  "odin_model",
  private = list(
    context = NULL,
    name = NULL,

    finalize = function() {
      private$context$eval("delete %s;", private$name)
    }
  ),

  public = list(
    initialize = function(context, generator) {
      private$context <- context
      private$name <- sprintf("%s.%s", JS_INSTANCES, basename(tempfile("i")))
      init <- sprintf("%s = new %s.%s();",
                      private$name, JS_GENERATORS, generator)
      private$context$eval(init)
    },

    initial = function(t) {
      private$context$call(sprintf("%s.initial", private$name), t)
    },

    deriv = function(t, y) {
      ## TODO: check length of 'y' here?
      private$context$call(sprintf("%s.rhs_eval", private$name), t, I(y))
    },

    run = function(t, y = NULL, ...) {
      if (is.null(y)) {
        y <- V8::JS("null")
      } else {
        y <- I(y)
      }
      ## TODO: need to decide what to do about names here, because
      ## we'll need to support sending metadata about the result back.
      ## I think that an option that controls name generation would be
      ## useful here perhaps?  Then a structure rather than a
      ## data.frame right from the get-go even.
      private$context$call(sprintf("%s.run", private$name), t, y)
    }
  ))
