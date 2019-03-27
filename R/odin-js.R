is## It's quite likely that we'll either roll this into the main package
## (which requires the v8 dependency a little more strongly, but
## possibly we can make that optional still) or we will make an
## extension mechanism (which would be useful if say Julia was to
## become a target later).
##
## Doing the latter requires some effort to keep the options
## extensible, but the current approach is a bit rubbish anyway.
odin_js <- function(x) {
  xx <- substitute(x)
  if (is.symbol(xx)) {
    xx <- force(x)
  } else if (odin:::is_call(xx, quote(c)) &&
             all(odin:::vlapply(xx[-1], is.character))) {
    ## See #88
    xx <- force(x)
  }
  odin_js_(xx)
}


odin_js_ <- function(x) {
  options <- odin::odin_options(target = "js")
  ir <- odin::odin_parse_(x, options)
  dat <- odin::odin_ir_deserialise(ir)
  ## For now we'll do one context per model, until mangling is done
  ## for the generators as well as the instances.
  ret <- generate_js(dat, options, js_context())
  ## class(ret) <- "odin_generator"
  ret
}
