odin_target_name <- function(using) {
  "js"
}


skip_for_target <- function(target, reason = NULL, using = NULL) {
  if (target == odin_target_name(using)) {
    if (is.null(reason)) {
      msg <- sprintf("Engine is %s", target)
    } else {
      msg <- sprintf("Engine is %s (%s)", target, reason)
    }
    testthat::skip(msg)
  }
}


skip_for_delay <- function() {
  skip("needs delay, not yet implemented in odin.js")
}
