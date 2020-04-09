options(odin.no_check_naked_index = TRUE)

sort_list <- function(x) {
  x[order(names(x))]
}


call_odin_bundle <- function(context, name, user, t, y = NULL) {
  context$eval(package_js("test.js"))
  user <- to_json(list(r = 0.5), auto_unbox = TRUE)
  if (is.null(y)) {
    y <- js_null()
  }
  res <- context$call("run", "odin", user, t, y)
  colnames(res$y) <- res$names
  res$y
}


odin_js_support <- function() {
  v8 <- V8::v8()
  support <- package_js("support.js")
  v8$eval(support)
  v8
}


expect_js_error <- function(...) {
  testthat::expect_error(..., class = "std::runtime_error")
}
