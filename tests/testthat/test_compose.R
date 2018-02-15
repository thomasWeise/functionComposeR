library("functionComposeR")
context("function.compose")

test_that("Test compose sin and cos", {
  f <- function.compose(sin, cos);
  x <- runif(n=10000)
  expect_identical(cos(sin(x)), f(x))
})

test_that("Test compose sin and user-provided function (I)", {
  p <- function(x) { 23*x }
  f <- function.compose(sin, p);
  x <- runif(n=10000)
  expect_identical(p(sin(x)), f(x))
})

test_that("Test compose sin and user-provided function (II)", {
  p <- function(x) { 23*x - x*x }
  f <- function.compose(sin, p);
  x <- runif(n=10000)
  expect_identical(p(sin(x)), f(x))
})

test_that("Test compose sin and user-provided function (III)", {
  k <- 23;
  p <- function(x) { k*x - x*x }
  f <- function.compose(sin, p);
  x <- runif(n=10000)
  expect_identical(p(sin(x)), f(x))
})

test_that("Test compose two user-provided functions (I)", {
  f1 <- function(x) { 3*x }
  f2 <- function(x) { 1/(5+x) }
  f <- function.compose(f1, f2);
  x <- runif(n=10000)
  expect_identical(f2(f1(x)), f(x))
})

test_that("Test compose two user-provided functions (II)", {
  f1 <- function(x) { 3*x }
  f2 <- function(x) { (x+1)/(5+x) }
  f <- function.compose(f1, f2);
  x <- runif(n=10000)
  expect_identical(f2(f1(x)), f(x))
})

test_that("Test compose two user-provided functions (III)", {
  k <- 3;
  f1 <- function(x) { k*x }
  j <- 7;
  f2 <- function(x) { 1/(j+x) }
  f <- function.compose(f1, f2);
  x <- runif(n=10000)
  expect_identical(f2(f1(x)), f(x))
})

test_that("Test compose two user-provided functions (IV)", {
  k <- 3;
  f1 <- function(x) { k*x }
  j <- 7;
  f2 <- function(x) { (x+1)/(j+x) }
  f <- function.compose(f1, f2);
  x <- runif(n=10000)
  expect_identical(f2(f1(x)), f(x))
})


test_that("Test compose two multi-argument functions (I)", {
  f1 <- function(x, y, z) { 0.77*x + 0.5*y + 0.2*z }
  f2 <- function(a, b, c) { b/(1/a + 7*c)  }

  f <- function.compose(f1, f2, f2g="a");
  expect_identical(f2(f1(1,2,3), 4, 5), f(4, 5, 1, 2, 3))
  expect_identical(f2(f1(-1,-2,-3), -4, -5), f(-4, -5, -1, -2, -3))

  f <- function.compose(f1, f2, f2g="b");
  expect_identical(f2(4, f1(1,2,3), 5), f(4, 5, 1, 2, 3))
  expect_identical(f2(-4, f1(-1,-2,-3), -5), f(-4, -5, -1, -2, -3))

  f <- function.compose(f1, f2, f2g="c");
  expect_identical(f2(4, 5, f1(1,2,3)), f(4, 5, 1, 2, 3))
  expect_identical(f2(-4, -5, f1(-1,-2,-3)), f(-4, -5, -1, -2, -3))
})


test_that("Test compose two multi-argument functions (II)", {
  f1 <- function(x, y, z) { 0.77*x + 0.5*y + 0.2*z }
  f2 <- function(a, b, c) { c*(b-((3-a)*b/(1/a + 7*c)))  }

  f <- function.compose(f1, f2, f2g="a");
  expect_identical(f2(f1(1,2,3), 4, 5), f(4, 5, 1, 2, 3))
  expect_identical(f2(f1(-1,-2,-3), -4, -5), f(-4, -5, -1, -2, -3))

  f <- function.compose(f1, f2, f2g="b");
  expect_identical(f2(4, f1(1,2,3), 5), f(4, 5, 1, 2, 3))
  expect_identical(f2(-4, f1(-1,-2,-3), -5), f(-4, -5, -1, -2, -3))

  f <- function.compose(f1, f2, f2g="c");
  expect_identical(f2(4, 5, f1(1,2,3)), f(4, 5, 1, 2, 3))
  expect_identical(f2(-4, -5, f1(-1,-2,-3)), f(-4, -5, -1, -2, -3))
})