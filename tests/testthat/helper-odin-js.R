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
