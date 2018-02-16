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


test_that("Test compose functions multiple times", {
  i<-45
  j<-33
  k<-23
  f <- function(x) { (x*(x-i)) - x/sinh(k*cos(j-atan(k+j))) }
  g <- function(x) { abs(x)^(abs(1/(3-i))) + (j - k*exp(-i)) / ((i*j) * x) }
  h.1.plain <- function(x) g(f(x))
  h.1.composed <- function.compose(f, g)
  h.2.plain <- function(x) g(f(g(f(x))))
  h.2.composed <- function.compose(function.compose(function.compose(f, g), f), g)
  x <- runif(1000)
  expect_identical(h.1.composed(x), h.1.plain(x))
  expect_identical(h.2.composed(x), h.2.plain(x))
})
