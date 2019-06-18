context("example")

## Full tests here would require selinium I believe
test_that("create example bundle", {
  model <- system.file("models/logistic.R", package = "odin.js")
  path <- odin_js_example(model, "simple")
  expect_setequal(dir(path), c("index.html", "odin.js"))
  ## The include process worked:
  js <- readLines(file.path(path, "odin.js"))
  expect_true(any(grepl("odinParameterId", js)))
  ct <- V8::v8()
  expect_true(ct$validate(js))
})


test_that("throw on missing example", {
  expect_error(odin_js_example("inst/models/logistic.R", "other"),
               "Unknown example 'other' - must be one of 'simple'")
})
