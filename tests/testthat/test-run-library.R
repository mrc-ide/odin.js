context("run: library support")

test_that("abs", {
  gen <- odin_js({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- abs(t)
  })
  tt <- seq(-5, 5, length.out = 101)
  expect_equal(gen$new()$run(tt)[, "a"], abs(tt))
})


test_that("log", {
  gen <- odin_js({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- log(t)
    output(b) <- log(t, 2)
    output(c) <- log(t, 10)
  })
  tt <- seq(0.0001, 5, length.out = 101)
  yy <- gen$new()$run(tt)
  expect_equal(yy[, "a"], log(tt))
  expect_equal(yy[, "b"], log2(tt))
  expect_equal(yy[, "c"], log10(tt))
})


test_that("pow", {
  gen <- odin_js({
    deriv(y) <- 0
    initial(y) <- 0
    output(a) <- min(t, t^2 - 2, -t)
    output(b) <- max(t, t^2 - 2, -t)
  })
  tt <- seq(0.0001, 5, length.out = 101)
  yy <- gen$new()$run(tt)
  expect_equal(yy[, "a"], pmin(tt, tt^2 - 2, -tt))
  expect_equal(yy[, "b"], pmax(tt, tt^2 - 2, -tt))
})


test_that("%%", {
  gen <- odin_js({
    deriv(y) <- 0
    initial(y) <- 0
    s <- sin(1) # does not appear exactly
    q <- 1.0    # appears exactly
    output(s1) <-  t %%  s
    output(s2) <- -t %%  s
    output(s3) <-  t %% -s
    output(s4) <- -t %% -s
    output(q1) <-  t %%  q
    output(q2) <- -t %%  q
    output(q3) <-  t %% -q
    output(q4) <- -t %% -q
  })
  tt <- seq(-5, 5, length.out = 101)
  mod <- gen$new()
  res <- mod$run(tt)
  s <- mod$contents()[["s"]]
  q <- mod$contents()[["q"]]

  expect_equal(res[, "s1"],  tt %%  s)
  expect_equal(res[, "s2"], -tt %%  s)
  expect_equal(res[, "s3"],  tt %% -s)
  expect_equal(res[, "s4"], -tt %% -s)

  expect_equal(res[, "q1"],  tt %%  q)
  expect_equal(res[, "q2"], -tt %%  q)
  expect_equal(res[, "q3"],  tt %% -q)
  expect_equal(res[, "q4"], -tt %% -q)
})


test_that("%/%", {
  ## As for %% but with %/%
  gen <- odin_js({
    deriv(y) <- 0
    initial(y) <- 0
    s <- sin(1) # does not appear exactly
    q <- 1.0    # appears exactly
    output(s1) <-  t %/%  s
    output(s2) <- -t %/%  s
    output(s3) <-  t %/% -s
    output(s4) <- -t %/% -s
    output(q1) <-  t %/%  q
    output(q2) <- -t %/%  q
    output(q3) <-  t %/% -q
    output(q4) <- -t %/% -q
  })
  tt <- seq(-5, 5, length.out = 101)
  mod <- gen$new()
  res <- mod$run(tt)
  s <- mod$contents()[["s"]]
  q <- mod$contents()[["q"]]

  expect_equal(res[, "s1"],  tt %/%  s)
  expect_equal(res[, "s2"], -tt %/%  s)
  expect_equal(res[, "s3"],  tt %/% -s)
  expect_equal(res[, "s4"], -tt %/% -s)

  expect_equal(res[, "q1"],  tt %/%  q)
  expect_equal(res[, "q2"], -tt %/%  q)
  expect_equal(res[, "q3"],  tt %/% -q)
  expect_equal(res[, "q4"], -tt %/% -q)
})


test_that("2-arg round", {
  gen <- odin_js({
    deriv(x) <- 1
    initial(x) <- 1
    output(y) <- TRUE
    output(z) <- TRUE
    n <- user(0)
    y <- round(t, n)
    z <- round(t)
  })

  mod0 <- gen$new(n = 0)
  mod1 <- gen$new(n = 1)
  mod2 <- gen$new(n = 2)

  tt <- seq(0, 1, length.out = 101)
  yy0 <- mod0$run(tt)
  yy1 <- mod1$run(tt)
  yy2 <- mod2$run(tt)

  expect_equal(yy0[, "z"], round(tt))
  expect_equal(yy1[, "z"], round(tt))
  expect_equal(yy2[, "z"], round(tt))

  expect_equal(yy0[, "y"], round(tt, 0))
  ## This needs a little work to work with R 4.0.0
  ## https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17668
  ## https://stat.ethz.ch/~maechler/R/Rounding.html
  ## expect_equal(yy1[, "y"], round(tt, 1))
  expect_equal(yy2[, "y"], round(tt, 2))
})
