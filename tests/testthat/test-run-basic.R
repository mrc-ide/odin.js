context("basic")

## Incomplete list taken from odin:run/test-basic.R

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
  expect_equal(yy[, 1], tt)
  expect_equal(yy[, 2], seq(1, length.out = length(tt), by = 2))

  expect_equal(mod$contents(), sort_list(list(initial_y = 1, r = 2)))
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


## Tests: that we can actually use state variables in a calculation
test_that("use state in derivative calculation", {
  gen <- odin_js({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- 1
    K <- 100
    r <- 2.0
  })

  mod <- gen()
  ## two equilibria:
  expect_equal(mod$deriv(0, 0), 0)
  expect_equal(mod$deriv(0, 100), 0)
  ## off equilibria:
  expect_equal(mod$deriv(0, 1), 2 * 1 * 99 / 100)
})


## Tests: multiple scalar variables can be packed together properly
test_that("multiple variables", {
  gen <- odin_js({
    deriv(y1) <- sigma * (y2 - y1)
    deriv(y2) <- R * y1 - y2 - y1 * y3
    deriv(y3) <- -b * y3 + y1 * y2
    initial(y1) <- 10.0
    initial(y2) <- 1.0
    initial(y3) <- 1.0
    sigma <- 10.0
    R     <- 28.0
    b     <-  8.0 / 3.0
  })
  mod <- gen()
  expect_equal(mod$initial(0), c(10, 1, 1))
  expect_equal(mod$deriv(0, mod$initial(0)), c(-90, 269, 22 / 3))
})


## Tests: scalar user variables
test_that("user variables", {
  gen <- odin_js({
    deriv(N) <- r * N * (1 - N / K)
    initial(N) <- N0
    N0 <- user(1)
    K <- 100
    r <- user()
  })

  expect_error(gen())
  ## TODO: had to change error strings here
  expect_error(gen(NULL),
               "Expected a value for 'r'", fixed = TRUE,
               class = "std::runtime_error")
  expect_error(gen(r = 1:2),
               "Expected a value for 'r'",
               class = "std::runtime_error")
  expect_error(gen(numeric(0)),
               "Expected a value for 'r'",
               class = "std::runtime_error")

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


## Tests: basic output
##
## This one is about as basic as I can see!
test_that("output", {
  skip("Needs implementing")
  gen <- odin_js({
    deriv(y) <- 2
    initial(y) <- 1
    output(z) <- t
  })

  tt <- 0:10

  mod <- gen()

  expect_equal(mod$deriv(0, 1), structure(2, output = 0))
  expect_equal(mod$deriv(10, 1), structure(2, output = 10))

  yy1 <- mod$run(tt)
  expect_equal(colnames(yy1), c("t", "y", "z"))
  expect_equal(yy1[, "t"], tt)
  expect_equal(yy1[, "y"], seq(1, length.out = length(tt), by = 2))
  expect_equal(yy1[, "z"], tt)

  yy2 <- gen(TRUE)$run(tt)
  expect_equal(colnames(yy2), c("t", "y", "z"))
  expect_equal(yy2[, "t"], tt)
  expect_equal(yy2[, "y"], seq(1, length.out = length(tt), by = 2))
  expect_equal(yy2[, "z"], tt)
})


## Do some nontrivial calculation in the output
test_that("output", {
  skip("needs implementing")
  gen <- odin_js({
    deriv(y) <- 2
    initial(y) <- 1
    output(z) <- a * 2
    a <- t + y
  })

  mod <- gen()
  expect_equal(mod$deriv(0, 1), structure(2, output = 2))
  expect_equal(mod$deriv(10, 1), structure(2, output = 22))
})


test_that("copy output", {
  skip("needs implementing")
  gen <- odin_js({
    deriv(y) <- 1
    initial(y) <- 1
    z[] <- t
    dim(z) <- 5
    output(z[]) <- TRUE
  })

  mod <- gen()
  tt <- 0:10
  y <- mod$run(tt)
  yy <- mod$transform_variables(y)
  expect_equal(yy$y, tt + 1)
  expect_equal(yy$z, matrix(tt, length(tt), 5))
})


## discrete
## discrete with output


## Fairly minimal array model, though it does mix array and non array
## variables, plus an array support variable.
test_that("array support", {
  gen <- odin_js({
    initial(x[]) <- 1
    initial(y) <- 2
    deriv(x[]) <- r[i]
    deriv(y) <- n
    r[] <- i
    n <- 3
    dim(r) <- n
    dim(x) <- n
  })

  mod <- gen()

  ## internal data is ok:
  expect_equal(sort_list(mod$contents()),
               sort_list(list(dim_r = 3, dim_x = 3, initial_x = rep(1, 3),
                              initial_y = 2, n = 3, r = 1:3)))
  expect_equal(mod$initial(0), c(2, 1, 1, 1))
  expect_equal(mod$deriv(0, c(2, 1, 1, 1)), c(3, 1, 2, 3))

  tt <- 0:10
  yy <- mod$run(tt)
  expect_equivalent(yy[, ],
                    cbind(tt, 2 + tt * 3, 1 + tt, 1 + tt * 2, 1 + tt * 3,
                          deparse.level = 0))
  expect_equal(colnames(yy), c("t", "y", "x[1]", "x[2]", "x[3]"))

  ## expect_equal(mod$transform_variables(yy),
  ##              list(t = tt,
  ##                   y = yy[, 2],
  ##                   x = unname(yy[, 3:5])))
})


test_that("multi-line array expression", {
  gen <- odin_js({
    initial(x) <- 1
    deriv(x) <- 1
    a[1] <- 1
    a[2] <- 1
    a[3:n] <- a[i - 1] + a[i - 2]
    dim(a) <- n
    n <- 10
  })
  expect_equal(gen()$contents()$a, c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55))
})
