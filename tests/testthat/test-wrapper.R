context("wrapper")


test_that("force a vector of strings", {
  gen <- odin_js(c("deriv(y) <- 0.5", "initial(y) <- 1"))
  mod <- gen()
  y <- mod$run(0:10)[, 1]
  expect_equal(y, seq(1, by = 0.5, length.out = 11))
})


test_that("force a symbol of code", {
  code <- quote({
    deriv(y) <- 0.5
    initial(y) <- 1
  })
  gen <- odin_js(code)
  mod <- gen()
  y <- mod$run(0:10)[, 1]
  expect_equal(y, seq(1, by = 0.5, length.out = 11))
})


test_that("allow initial conditions", {
  code <- quote({
    deriv(y) <- 0.5
    initial(y) <- 1
  })
  gen <- odin_js(code)
  mod <- gen()
  y <- mod$run(0:10, 2)[, 1]
  expect_equal(y, seq(2, by = 0.5, length.out = 11))
})
