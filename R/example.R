odin_js_example <- function(filename, name, dest = tempfile(), ...) {
  p <- system.file("example", package = "odin.js")
  if (!(name %in% dir(p))) {
    stop(sprintf("Unknown example '%s' - must be one of %s",
                 name, paste(squote(dir(p)))))
  }
  path <- file.path(p, name)
  include <- dir(path, pattern = "\\.js$", full.names = TRUE)
  html <- dir(path, pattern = "\\.html$", full.names = TRUE)

  dir.create(dest, FALSE, TRUE)
  odin_js_bundle(filename, file.path(dest, "odin.js"), include = include)
  file.copy(html, dest, overwrite = TRUE)
  dest
}
