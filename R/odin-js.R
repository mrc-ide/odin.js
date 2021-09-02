##' Generate a javascript odin model from a file, text string or
##' expression.
##'
##'
##' @title Create a javascript odin model
##'
##' @param x Either the name of a file to read, a text string (if
##'   length is greater than 1 elements will be joined with newlines)
##'   or an expression.
##'
##' @param ... Arguments passed to \code{\link{odin_options}}
##'
##' @param options An optional \code{\link{odin_options}} object. The
##'   \code{target} element (if present) will be overriden.
##'
##' @export
##' @importFrom odin odin
odin_js <- function(x, ..., options = NULL) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (odin:::is_call(xx, quote(c)) &&
             all(odin:::vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin_js_(xx, ..., options = options)
}


##' @export
##' @rdname odin_js
odin_js_ <- function(x, ..., options = NULL) {
  options <- odin::odin_options(..., options = options)
  options$target <- "js"
  ir <- odin::odin_parse_(x, options)
  odin_js_wrapper(ir, options)
}
