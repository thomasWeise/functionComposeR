# R Package for Composing and Canonicalizing Functions

[<img alt="Travis CI Build Status" src="https://img.shields.io/travis/thomasWeise/functionComposeR/master.svg" height="20"/>](https://travis-ci.org/thomasWeise/functionComposeR/)

## Introduction

This package provides four functions, namely

1. `expression.simplify`, which attempts to resolve as many sub-expressions of an expression as possible,
2. `function.canonicalize`, which tries to resolve the promises and variables inside a function, and
3. `function.compose`, which combines/chains two functions together.
4. `function.substitute`, which substitutes a specified subset of the formal parameters of a function with specified values, yielding a function of smaller arity

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
Of course, if you later try to inspect `h` and just write `h` in the R console,
you will see exactly this: `function(x) g(f(x))`.

This leads to two issues: First, if you do not know `f` and `g`, the output
is meaningless and opaque, you cannot interpret it.
Second, evaluating `h` is unnecessarily slow: It performs
two inner function calls and needs to evaluate a variable `k` at several
locations, although the value of `k` should be fixed to `23`.
Matter of fact, also `k*k` and `sin(k)` are constants which could
be known.

The goal of our new function `function.compose` is to resolve these two issues.
If you do `h<-function.compose(f, g)` instead of `h<-function(x) g(f(x))`, a
new function composed of both the bodies of `f` and `g` is created.
Furthermore, as many of the variables and expressions in the body which can
be resolved as possible are replaced by their constant result. Printing the
result of `h<-function.compose(f, g)` would yield the readable and fast function:

    function (x)
      {
        x <- (23 * x) + 7
        (529 - x * x)/(x - -0.846220404175171)
      }

Another component which might increase the speed of function execution is
the replacement of identical sub-expressions with the same object. This may
(or may not) be more cache friendly and therefore faster (or not).
    
    
## Installation

You can install the package directl from GitHub by using the package
[`devtools`](http://cran.r-project.org/web/packages/devtools/index.html) as
follows:

    library(devtools)
    install_github("thomasWeise/functionComposeR")

If `devtools` is not yet installed on your machine, you need to FIRST do

    install.packages("devtools")
    
If you get the error `Installation failed: NULL : 'rcmd_safe_env' is not an exported object from 'namespace:callr'` during the installation attempt, please do the following:

1. Install the package `callr` by typing `install.packages("callr")` in the `R` console and hitting enter.
2. Exit your `R` session and start `R` again.
3. Try the installation of `functionComposeR` again as described above.
    
## Usage

Here we give some use cases and additional examples for using the functions provided by our package.

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
    
### Substituting Function Parameters

    f <- function(x, pars) pars[1] + pars[2]*(x + pars[3]*x)
    f
    # function(x, pars) pars[1] + pars[2]*(x + pars[3]*x)
    g <- function.substitute(f, list("pars"=c(1, 2, 3)))
    g
    # function (x)
    #  1 + 2 * (x + 3 * x)
    h <- function.substitute(f, list("pars"=c(1, 2, 3), "x"=4))
    # function () 
    # 33
    
### Simplifying Expressions

    expression.simplify(5+3)
    # [1] 8
    k <- 8
    expression.simplify( ((5+3*k)-1)/k - 2^tan(k) )
    # [1] 3.491024
    f <- function(x) { 5+3+x }
    expression.simplify(body(f), envir=environment(f))
    # 8 + x

## Detailed Example
Let us now look at some more complex composed functions and also check the performance of the composed functions.

### Simple Composition

First, we again compose two functions which also access some variables from the environment.

    i<-45
    j<-33
    k<-23
    f <- function(x) { (x*(x-i)) - x/sinh(k*cos(j-atan(k+j))) }
    g <- function(x) { abs(x)^(abs(1/(3-i))) + (j - k*exp(-i)) / ((i*j) * x) }
    h.1.plain <- function(x) g(f(x))
    h.1.composed <- function.compose(f, g)

Printing `h.1.plain` and `h.1.composed` again reveals the difference between ordinary function composition in `R` and function composition using our `functionComposeR` package:

    h.1.plain
    # function(x) g(f(x))
    h.1.composed
    # function (x) 
    # {
    #     x <- (x * (x - 45)) - x/4818399372.40284
    #     abs(x)^0.0238095238095238 + 33/(1485 * x)
    # }
    
### Nested Function Composition

But we can also compose multiple functions, i.e., do

    h.2.plain <- function(x) g(f(g(f(x))))
    h.2.composed <- function.compose(function.compose(function.compose(f, g), f), g)

which yields functions of the form

    h.2.plain
    # function(x) g(f(g(f(x))))
    h.2.composed
    # function (x) 
    # {
    #     x <- {
    #         x <- {
    #             x <- (x * (x - 45)) - x/4818399372.40284
    #             abs(x)^0.0238095238095238 + 33/(1485 * x)
    #         }
    #         (x * (x - 45)) - x/4818399372.40284
    #     }
    #     abs(x)^0.0238095238095238 + 33/(1485 * x)
    # }
    
### Benchmarking

Let us finally evaluate the performance of the composed functions versus their plain counterparts. We therefore use the package [microbenchmark](http://cran.r-project.org/web/packages/microbenchmark/index.html).

    x <- runif(1000)
    library(microbenchmark)
    microbenchmark(h.1.plain(x), h.1.composed(x), h.2.plain(x), h.2.composed(x))
    # Unit: microseconds
    #             expr     min       lq      mean   median       uq     max neval
    #     h.1.plain(x)  78.841  79.4880  83.05224  79.9775  85.8485 119.824   100
    #  h.1.composed(x)  75.890  76.4675  93.23504  76.8385  78.9615 896.681   100
    #     h.2.plain(x) 153.793 154.8100 166.31210 155.5855 164.3685 743.360   100
    #  h.2.composed(x) 149.035 149.4870 155.25070 149.8895 154.0960 213.395   100

From the result, it becomes clearly visible that the upper quartiles of the runtime consumption of the composed functions are below the lower quartiles of the runtime consumption of their plain counterparts. Obviously, this very strongly depends on the example, but it is a clear indicator that our package can compose functions in way that is both human-readable and quick to evaluate.

## License

The copyright holder of this package is Prof. Dr. Thomas Weise (see Contact).
The package is licensed under the  GNU LESSER GENERAL PUBLIC LICENSE Version 3, 29 June 2007.

## Contact

For more information, see our corresponding [blog post](http://iao.hfuu.edu.cn/blogs/programming-blog/131).

If you have any questions or suggestions, please contact
[Prof. Dr. Thomas Weise](http://iao.hfuu.edu.cn/team/director) of the
[Institute of Applied Optimization](http://iao.hfuu.edu.cn/) at
[Hefei University](http://www.hfuu.edu.cn) in
Hefei, Anhui, China via
email to [tweise@hfuu.edu.cn](mailto:tweise@hfuu.edu.cn).
