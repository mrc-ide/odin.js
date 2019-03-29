## We're going to want to call these models from R a lot for testing,
## so let's expose them as full R objects:
odin_js_wrapper <- function(context, name, meta) {
  force(context)
  force(name)
  function(..., user = list(...)) {
    R6_odin_js_wrapper$new(context, name, user)
  }
}


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
      user_js <- to_json(user, auto_unbox = TRUE)
      init <- sprintf("%s = new %s.%s(%s);",
                      private$name, JS_GENERATORS, generator, user_js)
      private$context$eval(init)
    },

    initial = function(t) {
      t_js <- to_json(scalar(t))
      private$context$call(sprintf("%s.initial", private$name), t_js)
    },

    set_user = function(user) {
      user_js <- to_json(user, auto_unbox = TRUE)
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

    run = function(t, y = NULL, ...) {
      t_js <- to_json(t, auto_unbox = FALSE)
      if (is.null(y)) {
        y_js <- V8::JS("null")
      } else {
        y_js <- to_json(y, auto_unbox = FALSE)
      }
      ## TODO: need to decide what to do about names here, because
      ## we'll need to support sending metadata about the result back.
      ## I think that an option that controls name generation would be
      ## useful here perhaps?  Then a structure rather than a
      ## data.frame right from the get-go even.
      private$context$call(sprintf("%s.run", private$name), t_js, y_js)
    }
  ))
