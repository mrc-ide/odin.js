JS_GENERATORS <- "generators"
JS_INSTANCES <- "instances"


##' @importFrom V8 v8
js_context <- function() {
  ct <- V8::v8()
  ct$source(system.file("bundle.js", package = "odin.js", mustWork = TRUE))
  ct$source(system.file("support.js", package = "odin.js", mustWork = TRUE))
  ct$eval(sprintf("var %s = {};", JS_GENERATORS))
  ct$eval(sprintf("var %s = {};", JS_INSTANCES))
  ct
}
