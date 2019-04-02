context("odin-js")

test_that("trivial model", {
  gen <- odin_js({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2
  })

  mod <- gen()
  expect_is(mod, "odin_model")
  expect_equal(mod$initial(0), 1)
  expect_equal(mod$initial(10), 1)
  expect_equal(mod$deriv(0, 0), 2)
  expect_equal(mod$deriv(10, 10), 2)
  tt <- 0:10
  yy <- mod$run(tt)

  expect_equal(colnames(yy), c("t", "y"))
  expect_equal(yy[, "t"], tt)
  expect_equal(yy[, "y"], seq(1, length.out = length(tt), by = 2))

  expect_equal(sort_list(mod$contents()),
               sort_list(list(initial_y = 1, r = 2)))
})


## This tests a few things
##
## 1. can we use time dependent rhs
## 2. can we make transient variables work correctly
## 3. we can construct somewhat nontrivial expressions
##
## This should integrate to a parabola y = 1 + t^2
test_that("Time dependent rhs", {
  gen <- odin_js({
    deriv(y) <- r
    initial(y) <- 1
    r <- 2 * t
  })

  ## This looks like a reasonable rhs but it's going through the
  ## internal storage instead of being transient.
  mod <- gen()

  tt <- 0:10
  yy <- mod$run(tt, atol = 1e-8, rtol = 1e-8)
  expect_equal(yy[, 1], tt)
  expect_equal(yy[, 2], 1 + tt^2)

  expect_equal(mod$contents(), list(initial_y = 1))
})


test_that("Time dependent initial conditions", {
  gen <- odin_js({
    y1 <- cos(t)
    y2 <- y1 * (r + t)
    r <- 1
    deriv(y3) <- y2
    initial(y3) <- y2
  })

  mod <- gen()

  f <- function(t) {
    cos(t) * (1 + t)
  }

  expect_equal(mod$initial(0), f(0))
  expect_equal(mod$initial(1), f(1))
  expect_equal(mod$deriv(0, 1), f(0))
  expect_equal(mod$deriv(1, 1), f(1))

  expect_equal(sort_list(mod$contents()),
               sort_list(list(initial_y3 = f(1), r = 1)))
})


test_that("user variables", {
  gen <- odin_js({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user(1)
    K <- 100
    r <- user()
  })

  expect_error(gen())
  expect_error(gen(NULL),
               "Expected a value for 'r'", fixed = TRUE)
  ## expect_error(gen(1:2),
  ##              "Expected a scalar numeric for 'r'")
  ## expect_error(gen(numeric(0)),
  ##              "Expected a scalar numeric for 'r'")

  expect_equal(sort_list(gen(r = pi)$contents()),
               sort_list(list(K = 100, N0 = 1, initial_N = 1, r = pi)))
  expect_equal(sort_list(gen(r = pi, N0 = 10)$contents()),
               sort_list(list(K = 100, N0 = 10, initial_N = 10, r = pi)))
  expect_equal(gen(r = pi, N0 = 10)$initial(0), 10)
  expect_equal(gen(r = pi, N0 = 10)$deriv(0, 10),
               pi * 10 * (1 - 10 / 100))

  mod <- gen(r = pi, N0 = exp(1))
  mod$set_user(NULL)
  expect_equal(mod$contents()$r, pi)
  expect_equal(mod$contents()$N0, exp(1))
})
