# R Utilities for Composing and Canonicalizing Functions

[<img alt="Travis CI Build Status" src="https://img.shields.io/travis/thomasWeise/functionComposeR/master.svg" height="20"/>](https://travis-ci.org/thomasWeise/functionComposeR/)

## Introduction

This package provides three functions, namely

1. `expression.simplify`, which attempts to resolve as many sub-expressions of an expression as possible,
2. `function.canonicalize`, which tries to resolve the promises and variables inside a function, and
3. `function.compose` which combines/chains two functions together.

The goal is to be able to produce readable and quick-to-evaluate functions,
in particular to be able to compose functions iteratively without creating
more and more complex entities that depend on different environments and are
largely opaque. With this package, you can retain readble functions with
as many variables substituted by constants and sub-expressions evaluated to
constants as possible, which should also speed up the computation of the
functions.

## Motivating Example

As example, assume you have set `k <- 23` and now want to compose the
functions `f<-function(x) { (k*x) + 7 }` and
`g<-function(x) { (k*k-x*x) / (x - sin(k)) }` to a function `h`.
You can do that by writing `h<-function(x) g(f(x))`.
Of course, if you later try to inspect `h` and just write `h`, you will
see exactly this, `function(x) g(f(x))`.

This leads to two issues: First, if you do not know `f` and `g`, the output is meaningless and opaque, you cannot interpret it.
Second, evaluating `h` is unnecessarily slow: It performs
two inner function calls and needs to evaluate a variable `k` at several
locations, although the value of `k` should be fixed to `23`.
Matter of fact, also `k*k` and `sin(k)` are constants which could
be known.

The goal of `function.compose` is to resolve these two issues. If you do
`h<-function.compose(f, g)` instead of `h<-function(x) g(f(x))`, a
new function composed of both the bodies of `f` and `g` is created.
Furthermore, as many of the variables and expressions in the body which can
be resolved as possible are replaced by their constant result. Printing the
result of `h<-function.compose(f, g)` would yield the readable and fast function:

    function (x)
      {
        x <- (23 * x) + 7
        (529 - x * x)/(x - -0.846220404175171)
      }

    
## Installation

You can install the package directl from GitHub by using the package
[`devtools`](http://cran.r-project.org/web/packages/devtools/index.html) as
follows:

    library(devtools)
    install_github("thomasWeise/functionComposeR")

If `devtools` is not yet installed on your machine, you need to FIRST do

    install.packages("devtools")
    
## Usage

### Composing Functions

    f<-function(x,y,z=7,c) { x+23*y-z+2+c }
    g<-function(a=3,b=4,c) { a*b - b*c }
    function.compose(f, g, f2g="b")
    # function (a = 3, c, x, y, z = 7)
    # {
    #   b <- x + 23 * y - z + 2 + c
    #   a * b - b * c
    # }
    function.compose(sin, cos, f2g="x")
    # function (x)
    # cos(x = sin(x = x))
    k <- 23
    f2 <- function(x) { k*5 + x }
    g2 <- function(x) { x/(k - sin(k)) }
    function.compose(f2, g2)
    # function (x)
    # (115 + x)/23.8462204041752
    
    k<-23
    f<-function(x) { (k*x) + 7 }
    g<-function(x) { (k*k-x*x) / (x - sin(k)) }
    h<-function(x) g(f(x))
    h
    # function(x) g(f(x))
    h.composed<-function.compose(f, g)
    h.composed
    # function (x)
    # {
    #   x <- (23 * x) + 7
    #   (529 - x * x)/(x - -0.846220404175171)
    # }

### Canonicalizing Functions

    f <- function(x) { 5+3+x }
    function.canonicalize(f)
    # function (x)
    # 8 + x
    z <- 24;
    g <- function(x) { tan(sin(z) + (z*27) / x) }
    function.canonicalize(g)
    # function (x)
    # tan(-0.905578362006624 + 648/x)

### Simplifying Expressions

    expression.simplify(5+3)
    # [1] 8
    k <- 8
    expression.simplify( ((5+3*k)-1)/k - 2^tan(k) )
    # [1] 3.491024
    f <- function(x) { 5+3+x }
    expression.simplify(body(f), envir=environment(f))
    # 8 + x

## Contact

If you have any questions or suggestions, please contact
[Prof. Dr. Thomas Weise](http://iao.hfuu.edu.cn/team/director) of the
[Institute of Applied Optimization](http://iao.hfuu.edu.cn/) at
[Hefei University](http://www.hfuu.edu.cn) in
Hefei, Anhui, China via
email to [tweise@hfuu.edu.cn](mailto:tweise@hfuu.edu.cn).
