context("example")

## Full tests here would require selinium I believe
test_that("create example bundle", {
  path <- odin_js_example("inst/models/logistic.R", "simple")
  expect_setequal(dir(path), c("index.html", "odin.js"))
  ## The include process worked:
  js <- readLines(file.path(path, "odin.js"))
  expect_true(any(grepl("odinParameterId", js)))
  ct <- V8::v8()
  expect_true(ct$validate(js))
})
