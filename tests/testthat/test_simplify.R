library("functionComposeR")
context("expression.simplify")

test_that("simplify 4+5", {
  x <- expression.simplify(4+5)
  expect_identical(eval(x), 9)
  expect_identical(toString(x), "9")
})

test_that("simplify 4+5*k", {
  k <- 20;
  x <- expression.simplify(4+5*k)
  expect_identical(eval(x), 104)
  expect_identical(toString(x), "104")
})

test_that("simplify a function body with external constant", {
  k <- 20;
  f <- function(zzz) { 3 * zzz + (k*7) }
  v <- expression.simplify(body(f), environment(f))
  expect_identical(toString(v), "+, 3 * zzz, 140")
})

test_that("simplify a function body", {
  f <- function(zzz) { 3 * zzz + 7 }
  v <- expression.simplify(body(f), environment(f))
  expect_identical(toString(v), "+, 3 * zzz, 7")
})