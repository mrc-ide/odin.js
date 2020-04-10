context("support")

test_that("interpolateSearch", {
  ctx <- odin_js_support()

  helper <- c(
    "function testSearch(x, i, target) {",
    '  var obj = interpolateAlloc("constant", x, x, true);',
    "  obj.i = i;",
    "  return interpolateSearch(target, obj);",
    "}")
  ctx$eval(helper)

  test_interpolate_search <- function(x, i, target) {
    ctx$call("testSearch", x, i, target)
  }

  x <- 0:9 + 0.5

  ## off the edges
  expect_equal(test_interpolate_search(x, 0, 0), -1L)
  expect_equal(test_interpolate_search(x, 0, 10), 10L)
  expect_equal(test_interpolate_search(x, 5, 0), -1L)
  expect_equal(test_interpolate_search(x, 5, 10), 10L)
  expect_equal(test_interpolate_search(x, 9, 0), -1L)
  expect_equal(test_interpolate_search(x, 9, 10), 10L)

  x <- I(0.5)
  expect_equal(test_interpolate_search(x, 0, 0), -1)
  expect_equal(test_interpolate_search(x, 0, x), 1)
  expect_equal(test_interpolate_search(x, 0, x - 1e-7), 1)
  expect_equal(test_interpolate_search(x, 0, x + 1e-7), 1)
})


## Ported from cinterpolate - I might move the js code there too?
test_that("interpolation", {
  ctx <- odin_js_support()

  helper <- c(
    "function testInterpolate(x, y, xout, type) {",
    '  var obj = interpolateAlloc(type, x, y, false);',
    "  var ret = [];",
    "  for (var i = 0; i < xout.length; ++i) {",
    "    ret.push(interpolateEval(xout[i], obj));",
    "  }",
    "  return ret;",
    "}")
  ctx$eval(helper)

  test <- function(x, y, xout, type) {
    res <- ctx$call("testInterpolate", to_json_max(x), to_json_max(c(y)),
                    to_json_max(xout), type)
    if (storage.mode(res) == "logical" && all(is.na(res))) {
      storage.mode(res) <- "numeric"
    }
    if (is.matrix(y)) {
      matrix(res, c(length(xout), ncol(y)))
    } else {
      drop(res)
    }
  }

  set.seed(1)
  x <- as.numeric(0:5)
  y <- runif(length(x))
  ## This set of points here has the advantage that it:
  ##   a. is out of order so excercises the search function
  ##   b. includes all original time points
  xout <- sample(seq(0, 5, length.out = 101))
  ## Overshoot:
  xout_over <- c(xout, max(x) + 0.5)
  ## Undershoot
  xout_under <- c(xout, min(x) - 0.5)

  rapprox <- list(
    constant = function(x, y, xout) approx(x, y, xout, "constant"))
  type <- "constant"

  for (type in names(rapprox)) {
    ## We're all good except that the constant interpolation is not
    ## quite correct in the case of identical time matches.
    res_c <- test(x, y, xout, type)
    res_r <- rapprox[[type]](x, y, xout)$y

    expect_equal(res_c, res_r, tolerance = 1e-12)

    res_c <- test(x, cbind(y, deparse.level = 0), xout, type)
    expect_equal(dim(res_c), c(length(xout), 1))
    expect_equal(drop(res_c), res_r, tolerance = 1e-12)

    ## This is where we get messy.
    y2 <- cbind(y, y, deparse.level = 0)
    res_c2 <- test(x, y2, xout, type)
    expect_equal(dim(res_c2), c(length(xout), 2))
    expect_equal(res_c2[, 1], res_r, tolerance = 1e-12)
    expect_equal(res_c2[, 2], res_r, tolerance = 1e-12)

    y3 <- cbind(y, y * 2, deparse.level = 0)
    res_c3 <- test(x, y3, xout, type)
    expect_equal(dim(res_c2), c(length(xout), 2))
    expect_equal(res_c3[, 1], res_r, tolerance = 1e-12)
    expect_equal(res_c3[, 2], res_r * 2, tolerance = 1e-12)

    res_c4 <- test(x, y3, xout_over, type)
    i <- length(xout_over)
    if (type == "constant") {
      expect_equal(res_c4[i, ], y3[nrow(y3),])
    } else {
      expect_equal(res_c4[i, ], rep(NA_real_, ncol(y3)))
    }
    res_c5 <- test(x, y3, xout_under, type)
    i <- length(xout_under)
    expect_equal(res_c5[i, ], rep(NA_real_, ncol(y3)))

    res_c6 <- test(x, y3, xout_over[i], type)
    if (type == "constant") {
      expect_equal(drop(res_c6), y3[nrow(y3),])
    } else {
      expect_equal(drop(res_c6), rep(NA_real_, ncol(y3)))
    }

    expect_equal(drop(test(x, y3, xout_under[i], type)),
                 rep(NA_real_, ncol(y3)))
  }
})
