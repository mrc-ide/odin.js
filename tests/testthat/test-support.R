context("support")

test_that("isMissing", {
  ctx <- odin_js_support()
  expect_false(ctx$call("isMissing", 1))
  expect_false(ctx$call("isMissing", list(a = 1, b = 2)))
  expect_true(ctx$call("isMissing", V8::JS("null")))
})


test_that("getUserArray", {
  ctx <- odin_js_support()
  helper <- c(
    "function test(user, name, size, defaultValue, min, max, isInteger) {",
    "  var internal = {};",
    "  getUserArray(user, name, internal, size, defaultValue, min, max,",
    "               isInteger);",
    "  return internal[name];",
    "}")

  ctx$eval(helper)

  ## Get the required user data:
  user <- list(a = jsonlite::unbox(1),
               b = list(data = 1:6, dim = 2:3),
               c = list(data = 1:4, dim = I(4)))
  dim_b <- c(6, 2, 3)
  dim_c <- c(4, 4)
  dim_3 <- c(6, 2, 3, 1)

  null <- V8::JS("null")
  expect_equal(ctx$call("test", user, "b", dim_b, null, null, null, FALSE),
               1:6)
  expect_equal(ctx$call("test", user, "c", dim_c, null, null, null, FALSE),
               1:4)

  ## Check exists
  expect_error(
    ctx$call("test", user, "x", dim_b, null, null, null, FALSE),
    "Expected a value for 'x'",
    class = "std::runtime_error")

  ## Throw if not a matrix-type object
  expect_error(
    ctx$call("test", user, "a", dim_b, null, null, null, FALSE),
    "Expected an odin.js array object",
    class = "std::runtime_error")

  ## Check ranks
  expect_error(
    ctx$call("test", user, "b", dim_3, null, null, null, FALSE),
    "Expected a numeric array of rank 3 for 'b'",
    class = "std::runtime_error")
  expect_error(
    ctx$call("test", user, "b", dim_c, null, null, null, FALSE),
    "Expected a numeric vector for 'b'",
    class = "std::runtime_error")
  expect_error(
    ctx$call("test", user, "c", dim_b, null, null, null, FALSE),
    "Expected a numeric matrix for 'c'",
    class = "std::runtime_error")

  ## Check is a number
  user$b$data <- letters[1:6]
  expect_error(
    ctx$call("test", user, "b", dim_b, null, null, null, FALSE),
    "Expected a numeric value for 'b'",
    class = "std::runtime_error")

  ## Check range
  expect_error(
    ctx$call("test", user, "c", dim_c, null, 3, null, FALSE),
    "Expected 'c' to be at least 3",
    class = "std::runtime_error")
  expect_error(
    ctx$call("test", user, "c", dim_c, null, null, 3, FALSE),
    "Expected 'c' to be at most 3",
    class = "std::runtime_error")
})


test_that("getUserArrayDim", {
  ctx <- odin_js_support()
  helper <- c(
    "function test(user, name, rank, defaultValue, min, max, isInteger) {",
    "  var internal = {};",
    "  var size = new Array(rank + 1);",
    "  getUserArrayDim(user, name, internal, size, defaultValue, min, max,",
    "                  isInteger);",
    "  return {value: internal[name], size: size};",
    "}")

  ctx$eval(helper)

  ## Get the required user data:
  user <- list(a = jsonlite::unbox(1),
               b = list(data = 1:6, dim = 2:3),
               c = list(data = 1:6, dim = I(6)))
  null <- V8::JS("null")
  expect_equal(ctx$call("test", user, "b", 2, null, null, null, FALSE),
               list(value = 1:6, size = c(6, 2, 3)))
  expect_equal(ctx$call("test", user, "c", 1, null, null, null, FALSE),
               list(value = 1:6, size = c(6, 6)))

  ## Check exists
  expect_error(
    ctx$call("test", user, "x", 2, null, null, null, FALSE),
    "Expected a value for 'x'",
    class = "std::runtime_error")

  ## Throw if not a matrix-type object
  expect_error(
    ctx$call("test", user, "a", 2, null, null, null, FALSE),
    "Expected an odin.js array object",
    class = "std::runtime_error")

  ## Check ranks
  expect_error(
    ctx$call("test", user, "b", 3, null, null, null, FALSE),
    "Expected a numeric array of rank 3 for 'b'",
    class = "std::runtime_error")
  expect_error(
    ctx$call("test", user, "b", 1, null, null, null, FALSE),
    "Expected a numeric vector for 'b'",
    class = "std::runtime_error")
  expect_error(
    ctx$call("test", user, "c", 2, null, null, null, FALSE),
    "Expected a numeric matrix for 'c'",
    class = "std::runtime_error")

  ## Check is a number
  user$b$data <- letters[1:6]
  expect_error(
    ctx$call("test", user, "b", 2, null, null, null, FALSE),
    "Expected a numeric value for 'b'",
    class = "std::runtime_error")

  ## Check range
  expect_error(
    ctx$call("test", user, "c", 1, null, 3, null, FALSE),
    "Expected 'c' to be at least 3",
    class = "std::runtime_error")
  expect_error(
    ctx$call("test", user, "c", 1, null, null, 3, FALSE),
    "Expected 'c' to be at most 3",
    class = "std::runtime_error")
})


test_that("generate sum", {
  code <- unlist(lapply(2:8, generate_js_support_sum))
  expect_equal(code,
               readLines(system.file("support_sum.js", package = "odin.js")))
})
