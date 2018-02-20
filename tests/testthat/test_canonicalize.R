library("functionComposeR")
context("function.canonicalize")

#' Check that two functions have exactly the same interface
.functions.have.same.interface <- function(f, g) {
  expect_identical(is.function(f), TRUE)
  expect_identical(is.function(g), TRUE)
  primitive <- is.primitive(g)
  expect_identical(primitive, is.primitive(f))
  if(primitive) {
    args <- formals(args(g))
    expect_identical(args, formals(args(f)))
  } else {
    args <- formals(g)
    expect_identical(args, formals(f))
  }
  expect_identical(environment(f), environment(g));
}

test_that("Test canonicalize sin", {
  f <- sin
  g <- function.canonicalize(f)
  .functions.have.same.interface(f, g)
  x <- runif(n=10000)
  expect_identical(f(x), g(x))
})

test_that("Test canonicalize user-provided unary function", {
  f <- function(x) {
    (x*5)/7
  }
  g <- function.canonicalize(f)
  .functions.have.same.interface(f, g)
  x <- runif(n=10000)
  expect_identical(f(x), g(x))
})

test_that("Test canonicalize user-provided unary function with external constant", {
  k <- 23;
  f <- function(x) {
    (x*5)/k
  }
  g <- function.canonicalize(f)
  .functions.have.same.interface(f, g)
  x <- runif(n=10000)
  expect_identical(f(x), g(x))
})


test_that("Test canonicalize user-provided binary function with external constant", {
  k <- 23;
  f <- function(x, y=8) {
    c((x*5)/k, (y-k)+x)
  }
  g <- function.canonicalize(f)
  .functions.have.same.interface(f, g)
  x <- runif(n=10000)
  y <- runif(n=10000)
  expect_identical(f(x, y), g(x, y))
  expect_identical(f(x), g(x))
})


test_that("Test canonicalize user-provided ternary function with external constant", {
  k <- 23;
  f <- function(x=7, y=8, z) {
    c(z+(x*5)/k, z*(y-k)+x)
  }

  g <- function.canonicalize(f)
  .functions.have.same.interface(f, g)
  x <- runif(n=10000)
  y <- runif(n=10000)
  z <- runif(n=10000)
  res <- f(x, y, z)
  expect_identical(res, g(x, y, z))
  expect_identical(f(z=z), g(z=z))
  expect_identical(f(x=x, z=z), g(x=x, z=z))
  expect_identical(f(y=y, z=z), g(y=y, z=z))
  k <- 24;
  expect_false(identical(f(x, y, z), g(x, y, z)))
  expect_false(identical(f(z=z), g(z=z)))
  expect_false(identical(f(x=x, z=z), g(x=x, z=z)))
  expect_false(identical(f(y=y, z=z), g(y=y, z=z)))
  expect_false(identical(f(x, y, z), res))
  expect_identical(res, g(x, y, z))
})

test_that("Test canonicalize a constant function", {
  f <- function(x) 5
  g <- function.canonicalize(f)
  .functions.have.same.interface(f, g)
  expect_identical(f(1), g(1))
  expect_identical(g(1), 5)

  bwdv <- 7
  f <- function(x) bwdv
  g <- function.canonicalize(f)
  expect_identical(f(1), g(1))
  expect_identical(g(1), 7)
})


test_that("Test canonicalize nested functions with ... (I)", {
  k <- 23;
  f <- function(x, y) (x+(5/k)) - y
  g <- function(x, ...) f(x, ...)

  h <- function.canonicalize(g)
  .functions.have.same.interface(h, g)
  x <- runif(n=10000)
  y <- runif(n=10000)
  expect_identical(g(x, y), h(x, y))
})


test_that("Test canonicalize nested functions with ... (II)", {
  k <- 23;
  f <- function(x, y) (x+(5/k)) - y
  g <- function(...) f(...)

  h <- function.canonicalize(g)
  .functions.have.same.interface(h, g)
  x <- runif(n=10000)
  y <- runif(n=10000)
  expect_identical(g(x, y), h(x, y))
})


test_that("Test canonicalize nested functions with ... (III)", {
  k <- 23;
  f <- function(x, y) (x+(5/k)) - y
  g <- function(...) f(...) - k/f(...)

  h <- function.canonicalize(g)
  .functions.have.same.interface(h, g)
  x <- runif(n=10000)
  y <- runif(n=10000)
  expect_identical(g(x, y), h(x, y))
})


test_that("Test canonicalize nested functions with ... (IV)", {
  k <- 23;
  f <- function(x, y) (x+(5/k)) - y
  g <- function(...) f(f(...) - k/f(...), k*23)

  h <- function.canonicalize(g)
  .functions.have.same.interface(h, g)
  x <- runif(n=10000)
  y <- runif(n=10000)
  expect_identical(g(x, y), h(x, y))
})


test_that("Test canonicalize functions with vector constants", {
  k <- c(1, 2, 3, 4);
  f <- function(x) x*k

  g <- function.canonicalize(f)
  .functions.have.same.interface(f, g)
  x <- runif(n=length(k))
  expect_identical(f(x), g(x))
})

test_that("Test canonicalize functions with vector constants", {
  k <- c(1, 2, 3, 4);
  f <- function(x, y) (x+(5/k)) - y
  g <- function(...) f(f(...) - k/f(...), k*23)

  h <- function.canonicalize(g)
  .functions.have.same.interface(h, g)
  x <- runif(n=length(k))
  y <- runif(n=length(k))
  expect_identical(g(x, y), h(x, y))
})
