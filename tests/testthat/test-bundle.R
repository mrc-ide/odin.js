context("bundle")

test_that("bundle works", {
  code <- c("deriv(N) <- r * N * (1 - N / K)",
            "initial(N) <- N0",
            "N0 <- user(1)",
            "K <- 100",
            "r <- user()")
  p <- tempfile()
  dir.create(p)
  filename <- file.path(p, "odin.R")
  writeLines(code, filename)

  res <- odin_js_bundle(filename)

  ct <- V8::v8()
  invisible(ct$source(res))

  t <- 0:10
  res <- call_odin_bundle(ct, "odin", list(r = 0.5), t)
  sol <- with(list(K = 100, r = 0.5, y0 = 1),
              K / (1 + (K / y0 - 1) * exp(-r * t)))

  expect_equal(res[, "t"], t)
  expect_equal(res[, "N"], sol, tolerance = 1e-6)
})


test_that("bundle fails with missing files", {
  path1 <- file.path(tempdir(), "myfile1.R")
  path2 <- file.path(tempdir(), "myfile2.R")
  expect_error(odin_js_bundle(path1), "File does not exist:")
  expect_error(odin_js_bundle(c(path1, path2)), "Files do not exist:")
})


test_that("unique model names", {
  path1 <- tempfile()
  path2 <- tempfile()

  code <- c("deriv(y) <- 1", "initial(y) <- 1", 'config(base) <- "test"')
  writeLines(code, path1)
  writeLines(code, path2)

  expect_error(odin_js_bundle(c(path1, path2)),
               "Duplicate model names: 'test'")
})
