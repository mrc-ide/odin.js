context("run: general")

test_that("conditionals, precendence", {
  gen <- odin_js({
    deriv(x) <- 0.1 + 2 * if (t > 2) -0.1 else 0.5
    initial(x) <- 1
  })

  mod <- gen()
  t <- seq(0, 5, length.out = 101)
  y <- mod$run(t)

  cmp <- ifelse(t < 2, 1.1 * t, 2.4 -0.1 * t) + 1
  expect_equal(y[, 2], cmp, tolerance = 1e-4)
})
