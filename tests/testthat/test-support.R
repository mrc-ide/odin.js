context("support")

test_that("isMissing", {
  ctx <- odin_js_support()
  expect_false(ctx$call("isMissing", 1))
  expect_false(ctx$call("isMissing", list(a = 1, b = 2)))
  expect_true(ctx$call("isMissing", V8::JS("null")))
})
