odin_js_bundle <- function(filenames, dest = tempfile(),
                           include_dopri = TRUE) {
  ## The two options here seem to be: use a vector of paths to source
  ## files or use a path to a directory.  The rest of the interface is
  ## also totally subject to change because we might want to move
  ## those options into the general options interface.  The option to
  ## directly minify from R via V8 would be nice too but probably is
  ## not possible because of the way that the minification process
  ## involves the disk.
  err <- !file.exists(filenames)
  if (any(err)) {
    stop(sprintf("%s not exist: %s",
                 ngettext(sum(err), "File does", "Files do"),
                 paste(squote(filenames[err]), collapse = ", ")))
  }

  options <- odin::odin_options(target = "js")

  f <- function(file) {
    ir <- odin::odin_parse_(file)
    dat <- odin::odin_ir_deserialise(ir)
    code <- generate_js_code(dat, options)
    list(name = dat$config$base, file = file, code = code)
  }

  dat <- lapply(filenames, f)

  nms <- vcapply(dat, "[[", "name")
  err <- duplicated(nms)
  if (any(err)) {
    stop(sprintf("Duplicate model names: %s",
                 paste(squote(unique(nms[err]))), collapse = ", "))
  }

  dopri <- if (include_dopri) package_js("dopri.js") else NULL
  support <- package_js("support.js")
  code <- c(dopri,
            support,
            sprintf("var %s = {};", JS_GENERATORS),
            js_flatten_eqs(lapply(dat, "[[", "code")))

  writeLines(code, dest)
  dest
}
