library("functionComposeR")
context("function.substitute")

test_that("substitute a vector into a function", {
  f <- function(x, pars) pars[1] + pars[2]*(x + pars[3]*x)
  g <- function.substitute(f=f, list("pars"=c(1, 2, 3)))
  x <- runif(10000)
  expect_identical(g(x), f(x, c(1,2,3)))
  expect_identical(toString(body(g)), "+, 1, 2 * (x + 3 * x)")
})

test_that("substitute a variable and a vector into a function", {
  f <- function(x, pars) pars[1] + pars[2]*(x + pars[3]*x)
  g <- function.substitute(f=f, list("pars"=c(1, 2, 3), "x"=4))
  x <- runif(10000)
  expect_identical(g(), f(4, c(1,2,3)))
  expect_identical(toString(body(g)), "33")
})
