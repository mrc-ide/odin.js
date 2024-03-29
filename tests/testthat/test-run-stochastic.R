context("run:stochastic")

test_that("stochastic", {
  skip_if_no_random_js()
  ## Here's a stochastic random walk:
  gen <- odin_js({
    initial(x) <- 0
    update(x) <- x + norm_rand()
  })

  mod <- gen$new()
  tt <- 0:20
  model_set_seed(mod, 1)
  yy1 <- mod$run(tt)

  model_set_seed(mod, 1)
  cmp <- model_random_numbers(mod, "normal", length(tt) - 1L)
  expect_equal(cumsum(c(0, cmp)), yy1[, "x"])

  ## Repeatable
  model_set_seed(mod, 1)
  yy2 <- mod$run(tt)
  expect_equal(yy1, yy2)
})


## I'm not totally sure what the right call is here.  If I make a
## variable that is used only in the initial condition I do not want
## that repeatedly called during the run.
test_that("stochastic variables are time dependent", {
  skip_if_no_random_js()
  gen <- odin_js({
    v <- norm_rand() # this variable is implicitly time dependent.
    initial(x) <- 0
    update(x) <- x + v
  })

  mod <- gen$new()
  tt <- 0:20
  model_set_seed(mod, 1)
  yy1 <- mod$run(tt)

  model_set_seed(mod, 1)
  cmp <- model_random_numbers(mod, "normal", length(tt) - 1L)
  expect_equal(cumsum(c(0, cmp)), yy1[, "x"])
})


test_that("array stochastic variables are time dependent", {
  skip_if_no_random_js()
  ## This checks that even in the absence of array indexing on the RHS
  ## array variables are set correctly when stochastic.
  gen <- odin_js({
    initial(x[]) <- 0
    update(x[]) <- norm_rand()
    dim(x) <- 3
  })

  mod <- gen$new()
  tt <- 0:20
  model_set_seed(mod, 1)
  yy <- mod$run(tt)
  zz <- mod$transform_variables(yy)
  model_set_seed(mod, 1)
  cmp <- rbind(0,
               matrix(model_random_numbers(mod, "normal", 3 * 20), 20, 3, TRUE))
  expect_equal(zz$x, cmp)
})


test_that("stochastic initial conditions don't get called every step", {
  skip_if_no_random_js()
  ## There is quite a few nasty little conditions that are tested
  ## here.
  gen <- odin_js({
    v <- norm_rand() # this variable is implicitly time dependent.
    initial(x) <- v
    update(x) <- x + 1
  })

  ## cmp <- .Random.seed
  mod <- gen$new()
  ## expect_equal(.Random.seed, cmp)

  ## Initial conditions (why is $init even a member here?)
  ## expect_null(mod$init)

  ## Re-running the initial conditions gives different answers:
  x0 <- mod$initial(0L)
  ## expect_false(identical(.Random.seed, cmp))
  expect_true(mod$initial(0L) != x0)

  ## Run the model from scratch
  tt <- 0:20
  model_set_seed(mod, 1)
  yy1 <- mod$run(tt)
  z <- model_random_numbers(mod, "normal", 1)

  ## First number drawn from distribution, leaving RNG moved forward
  ## by a single normal draw:
  model_set_seed(mod, 1)
  cmp <- model_random_numbers(mod, "normal", 2)
  expect_equal(yy1[, "x"], cmp[[1]] + tt)
  expect_equal(z, cmp[[2]])

  ## Don't advance the seed if not hitting the initial conditions.
  ## cmp <- .Random.seed
  ## expect_equal(mod$run(tt, 0)[, "x"], as.numeric(0:20))
  ## expect_equal(mod$run(tt, 1)[, "x"], as.numeric(1:21))
  ## expect_equal(.Random.seed, cmp)
})


test_that("exotic stochastic functions", {
  skip_if_no_random_js()
  gen <- odin_js({
    initial(x) <- 0
    mu <- 1
    sd <- 2
    update(x) <- rnorm(mu, sd)
  })

  mod <- gen$new()
  model_set_seed(mod, 1)
  y <- mod$run(0:10)

  model_set_seed(mod, 1)
  expect_equal(y[-1, "x"], model_random_numbers(mod, "normal", 10, 1, 2))
})


test_that("round & rbinom", {
  skip_if_no_random_js()
  gen <- odin_js({
    size <- user()
    p <- user()
    update(x) <- 0
    initial(x) <- rbinom(size, p)
  })

  mod <- gen$new(p = 1, size = 0.4)
  expect_equal(mod$initial(0), 0)
  mod$set_user(p = 1, size = 1.7)
  expect_equal(mod$initial(0), 2)
})


test_that("mutlinomial", {
  skip_if_no_random_js()
  skip("multinomial not supported")
  ## This is just a check that these compile and run
  sir1 <- odin_js("stochastic/sir_discrete.R")
  sir2 <- odin_js("stochastic/sir_discrete_stochastic.R")
  sir3 <- odin_js("stochastic/sir_discrete_stochastic2.R")
  sir4 <- odin_js("stochastic/sir_discrete_stochastic_multi.R")

  mod1 <- sir1()
  mod2 <- sir2()
  mod3 <- sir3()
  mod4 <- sir4()

  t <- 0:100
  y1 <- mod1$run(t)
  y2 <- mod2$run(t)
  y3 <- mod3$run(t)
  y4 <- mod4$run(t)

  ## TODO: these need real tests!  At the moment I just want to
  ## confirm that they run.
  expect_true(TRUE)
})


test_that("replicate: scalar", {
  skip_if_no_random_js()
  skip("replicate not supported")
  ## TODO: this will be a nice version to try and benchmark the dde
  ## overheads I think...
  gen <- odin_js({
    initial(x) <- 0
    update(x) <- x + norm_rand()
  })
  m <- gen$new()
  tt <- 0:50
  res <- m$run(tt, replicate = 100)
  yy <- m$transform_variables(res)
  expect_equal(names(yy), c("t", "x"))
  expect_equal(yy[[1]], tt)
  expect_equal(dim(yy[[2]]), c(51, 100))
  expect_equal(yy[[1]], res[, 1, 1])
  expect_equal(yy[[2]], res[, 2, ])
})


test_that("replicate: array", {
  skip_if_no_random_js()
  skip("replicate not supported")
  gen <- odin_js({
    initial(x) <- 0
    initial(y[]) <- 0
    update(x) <- x + norm_rand()
    update(y[]) <- y[i] + norm_rand() / 2
    dim(y) <- 3
  })
  m <- gen$new()

  tt <- 0:20
  res <- m$run(tt, replicate = 30)
  yy <- m$transform_variables(res)
  expect_equal(names(yy), c("t", "x", "y"))
  expect_equal(yy[[1]], tt)
  expect_equal(dim(yy[[2]]), c(21, 30))
  expect_equal(dim(yy[[3]]), c(21, 3, 30))
  expect_equal(yy[[1]], res[, 1, 1])
  expect_equal(yy[[2]], res[, 2, ])
  expect_equal(yy[[3]], unname(res[, 3:5, ]))
})


test_that("low-level stochastics: norm_rand", {
  skip_if_no_random_js()
  gen <- odin_js({
    initial(y) <- 0
    update(y) <- norm_rand()
  })
  m <- gen$new()

  tt <- 0:10
  model_set_seed(m, 1)
  y <- m$run(tt)[-1, "y"]

  model_set_seed(m, 1)
  expect_equal(y, model_random_numbers(m, "normal", 10))
})


test_that("low-level stochastics: unif_rand", {
  skip_if_no_random_js()
  gen <- odin_js({
    initial(y) <- 0
    update(y) <- unif_rand()
  })
  m <- gen$new()

  tt <- 0:10
  model_set_seed(m, 1)
  y <- m$run(tt)[-1, "y"]

  model_set_seed(m, 1)
  expect_equal(y, model_random_numbers(m, "uniform", 10))
})


test_that("low-level stochastics: exp_rand", {
  skip_if_no_random_js()
  gen <- odin_js({
    initial(y) <- 0
    update(y) <- exp_rand()
  })
  m <- gen$new()

  tt <- 0:10
  model_set_seed(m, 1)
  y <- m$run(tt)[-1, "y"]

  model_set_seed(m, 1)
  expect_equal(y, model_random_numbers(m, "exponential", 10))
})


test_that("rexp parametrisation", {
  skip_if_no_random_js()
  gen <- odin_js({
    initial(y) <- 0
    update(y) <- rexp(10)
  })
  m <- gen$new()

  tt <- 0:10
  model_set_seed(m, 1)
  y <- m$run(tt)[-1, "y"]

  model_set_seed(m, 1)
  expect_equal(y, model_random_numbers(m, "exponential", 10, 10))
})
