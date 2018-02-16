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

## Contact

[Prof. Dr. Thomas Weise](http://iao.hfuu.edu.cn/team/director)
[Institute of Applied Optimization](http://iao.hfuu.edu.cn/)
[Hefei University](http://www.hfuu.edu.cn)
Hefei, Anhui, China
Email: [tweise@hfuu.edu.cn](mailto:tweise@hfuu.edu.cn)
