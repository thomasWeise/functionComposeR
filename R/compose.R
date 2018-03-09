#' @importFrom pryr modify_lang make_function
#' @importFrom stringr str_count str_detect
#' @importFrom rlang as_closure

#' @include canonicalize.R
#' @include simplify.R

#' @title Concatenate Two Functions
#'
#' @description Try to find a small and efficient representation of the
#' concatenation of to functions \code{f} and \code{g} such that the resulting
#' function has the form \code{h(...)=g(...,f(...),...)} (where \code{...} stands
#' for an arbitrary numer of arguments, not the \code{R}-style dots).
#'
#' More precisely: The input are two functions \code{f} and \code{g} and a
#' "bridge argument" \code{f2g}. Both functions may have an arbirary argument
#' list, say \code{f(x,y,z=7,c)} and \code{g(a=3,b=4,c)}. The "bridge argument"
#' can be any parameter of \code{g}, let's say \code{b}. The result of
#' \code{function.compose(f, g, f2g="b")} will then be a function
#' \code{h(a=3,c,x,y,z=7)} which is equivalent to \code{g(a, f(x, y, z, c), c)}.
#'
#' Since we also canonicalize all functions (\code{f}, \code{g}, and \code{h})
#' involved, this means also that we attempt to substitute all potentially
#' substitutable variables and simplify the inner expressions as much as
#' possible.
#'
#' This is particularly useful when iteratively composing functions and wanting
#' to retain their readability and making the result as fast to execute as
#' possible. However, the behavior may be undefined if the parameters of the
#' function can be resolved as constant values in the current environment or
#' if functions with otherwise odd parameters are successfively composed.
#'
#' If the result of \code{f} is used exactly once in \code{g}, we substitute
#' the body of \code{f} into \code{g} to directly replace the bridge parameter.
#' If it is used more than once, we first shovel its result into a variable,
#' namely the bridge parameter.
#'
#' As example, assume you have set \code{k <- 23} and now want to compose the
#' functions \code{f<-function(x) { (k*x) + 7 }} and \code{g<-function(x) {
#' (k*k-x*x) / (x - sin(k)) }} to a function \code{h}. You can do that by
#' writing \code{h<-function(x) g(f(x))}. Of course, if you later try to inspect
#' \code{h} and just write \code{h}, you will see exactly this,
#' \code{function(x) g(f(x))}. This leads to two issues: First, if you do not
#' know \code{f} and \code{g}, the output is meaningless and opaque, you cannot
#' interpret it. Second, evaluating \code{h} is unnecessarily slow: It performs
#' two inner function calls and needs to evaluate a variable \code{k} at several
#' locations, although the value of \code{k} should be fixed to \code{23}.
#' Matter of fact, also \code{k*k} and \code{sin(k)} are constants which could
#' be known.
#'
#' The goal of \code{function.compose} is to resolve these two issues. If you do
#' \code{h<-function.compose(f, g)} instead of \code{h<-function(x) g(f(x))}, a
#' new function composed of both the bodies of \code{f} and \code{g} is created.
#' Furthermore, as many of the variables and expressions in the body which can
#' be resolved as possible are replaced by their constant result. Printing the
#' result of \code{h<-function.compose(f, g)} would yield (something like)
#' \code{function (x) { x <- (23 * x) + 7; (529 - x * x)/(x -
#' -0.846220404175171) }}.
#'
#' Additionally, we replace identical sub-expressions in the composed function
#' by the same object. This may be advantegous for a slightly faster execution
#' due to being more cache friendly.
#'
#' @param f the inner function
#' @param g the outer function
#' @param f2g the argument of \code{g} to be replaced with the return value of \code{f}
#' @export function.compose
#'
#' @examples
#' f<-function(x,y,z=7,c) { x+23*y-z+2+c }
#' g<-function(a=3,b=4,c) { a*b - b*c }
#' function.compose(f, g, f2g="b")
#' # function (a = 3, c, x, y, z = 7)
#' # {
#' #   b <- x + 23 * y - z + 2 + c
#' #   a * b - b * c
#' # }
#' function.compose(sin, cos, f2g="x")
#' # function (x)
#' # cos(x = sin(x = x))
#' k <- 23
#' f2 <- function(x) { k*5 + x }
#' g2 <- function(x) { x/(k - sin(k)) }
#' function.compose(f2, g2)
#' # function (x)
#' # (115 + x)/23.8462204041752
#' k<-23
#' f<-function(x) { (k*x) + 7 }
#' g<-function(x) { (k*k-x*x) / (x - sin(k)) }
#' h.plain<-function(x) g(f(x))
#' h.plain
#' # function(x) g(f(x))
#' h.composed<-function.compose(f, g)
#' h.composed
#' # function (x)
#' # {
#' #   x <- (23 * x) + 7
#' #   (529 - x * x)/(x - -0.846220404175171)
#' # }
#' i<-45
#' j<-33
#' k<-23
#' f <- function(x) { (x*(x-i)) - x/sinh(k*cos(j-atan(k+j))) }
#' g <- function(x) { abs(x)^(abs(1/(3-i))) + (j - k*exp(-i)) / ((i*j) * x) }
#' h.1.plain <- function(x) g(f(x))
#' h.1.plain
#' # function(x) g(f(x))
#' h.1.composed <- function.compose(f, g)
#' h.1.composed
#' # function (x)
#' # {
#' #     x <- (x * (x - 45)) - x/4818399372.40284
#' #     abs(x)^0.0238095238095238 + 33/(1485 * x)
#' # }
#' h.2.plain <- function(x) g(f(g(f(x))))
#' h.2.plain
#' # function(x) g(f(g(f(x))))
#' h.2.composed <- function.compose(function.compose(function.compose(f, g), f), g)
#' h.2.composed
#' # function (x)
#' # {
#' #     x <- {
#' #         x <- {
#' #             x <- (x * (x - 45)) - x/4818399372.40284
#' #             abs(x)^0.0238095238095238 + 33/(1485 * x)
#' #         }
#' #         (x * (x - 45)) - x/4818399372.40284
#' #     }
#' #     abs(x)^0.0238095238095238 + 33/(1485 * x)
#' # }
#' x <- runif(1000)
#' library(microbenchmark)
#' microbenchmark(h.1.plain(x), h.1.composed(x), h.2.plain(x), h.2.composed(x))
#' # Unit: microseconds
#' #             expr     min       lq      mean   median       uq     max neval
#' #     h.1.plain(x)  78.841  79.4880  83.05224  79.9775  85.8485 119.824   100
#' #  h.1.composed(x)  75.890  76.4675  93.23504  76.8385  78.9615 896.681   100
#' #     h.2.plain(x) 153.793 154.8100 166.31210 155.5855 164.3685 743.360   100
#' #  h.2.composed(x) 149.035 149.4870 155.25070 149.8895 154.0960 213.395   100
function.compose <- function(f, g, f2g="x") {
  .function.compose(f, g, f2g, .cache.make())
}

# The implementation of function.compose
.function.compose <- function(f, g, f2g="x", cache) {
  # First, we canonicalize g
  g <- .function.canonicalize(f=g, cache=cache);
  g <- base::force(g);

  # If f is null or f2g is null, we just return the canonicalized g.
  if(base::is.null(f) || base::is.null(f2g)) {
    return(g);
  }

  # We then try to extract the body and arguments of function g
  g.is.primitive <- base::is.primitive(g);
  if(g.is.primitive) {
    g.args <- base::formals(base::args(g));
    g.body <- base::body(rlang::as_closure(g));
  } else {
    g.args <- base::formals(g);
    g.body <- base::body(g);
  }

  # If the g has no arguments, then the return value of f is irrelevant and we
  # can return g directly.
  if(base::is.null(g.args)) {
    return(g);
  }
  g.body <- base::force(g.body);
  g.args <- base::force(g.args);
  g.args.names <- base::names(g.args);
  g.args.names <- base::force(g.args.names);

  # Now we check where the "bridge argument" f2g of g which should be
  # replaced by the return value of f occurs in the argument list of g
  h.discovery <- stringr::str_count(f2g, g.args.names);
  if(base::sum(h.discovery) <= 0L) {
    # It does not occur, so this means that f plays no role in g and g
    # can be returned as is
    return(g);
  }

  # We now canonicalize f
  f <- .function.canonicalize(f=f, cache=cache);
  f <- base::force(f);

  # We then try to extract the body and arguments of function f
  if(base::is.primitive(f)) {
    f.args <- base::formals(base::args(f));
    f.body <- base::body(rlang::as_closure(f));
  } else {
    f.args <- base::formals(f);
    f.body <- base::body(f);
  }
  f.body <- base::force(f.body);
  f.args <- base::force(f.args);
  f.args.names <- base::names(f.args);
  f.args.names <- base::force(f.args.names);

  # The return value of f is used in g.
  # We now need to construct a new argument list for h.
  if(base::is.null(f.args)) {
    # OK, f has no arguments, so its argument list cannot contain the bridge argument.
    f.discovery <- NULL;
  } else {
    # f has some arguments, so we scan for the bridge argument.
    f.discovery <- stringr::str_count(f2g, f.args.names);
  }

  # Does the agument list of f contain the bridge argument itself?
  if((base::is.null(f.discovery)) || (base::sum(f.discovery) <= 0)) {
    # This argument list does not contain the bridge argument if
    # f does not contain it either.
    h.args <- g.args[!h.discovery];
    h.args.names <- g.args.names[!h.discovery];
  } else {
    # f has an argument with the same name as the bridge argument.
    # In this case, we leave it at the original position in h but copy the default
    # value from f.
    h.args <- g.args;
    h.args[h.discovery] <- f.args[f.discovery];
    h.args.names <- g.args.names;
  }

  # Now we add all arguments of f which are not already arguments of g
  # to the final argument list
  if(!(base::is.null(f.args))) {
    for(i in 1:length(f.args)) {
     f.arg.name <- f.args.names[[i]];
      if(sum(stringr::str_detect(f.arg.name, h.args.names)) <= 0) {
        # We add the argument only if it is not part of the argument list of h
        h.args[[length(h.args)+1]] <- f.args[[i]];
        h.args.names <- c(h.args.names, f.arg.name);
      }
    }
  }

  h.args <- base::force(h.args);
  h.args.names <- base::force(h.args.names);
  # The list h.args should now contain all arguments of f and g,
  # without the bridge argument, but with default values, if any

  # We need the bridge argument as name.
  f2g.as.name <- base::as.name(f2g);

  # We now want to test whether we can replace f2g directly with the body of f.
  # This can only be done if g is not a primitive function.
  counter <- 0L;

  # We test how often exactly the bridge parameter occurs in g.
  pryr::modify_lang(x=g.body, f=function(x, envir) {
    if (base::is.name(x) && base::identical(x, f2g.as.name)) {
      base::assign("counter", (base::get("counter", envir=envir) + 1L), envir=envir);
    }
    return(x);
  }, envir=base::environment() );

  # OK, we have counted the occurences of f2g in the body of g.
  if(counter == 1L) {
    # f2g occurs exactly once if the body of g.
    # This means we can replace the occurence of f2g directly with the body of f.
    h.body <- pryr::modify_lang(x=g.body, f=function(x) {
      if (base::is.name(x) && base::identical(x, f2g.as.name)) {
        return(f.body);
      }
    return(x);
    } );
  } else {
    # OK, either the bridge argument appears more than once or g is a primitive function.
    # In both cases, we need to create a new function which first stores the results of
    # f in a variable and then returns the result of g.

    # This is our function blueprint.
    .temp.2 <- NULL;
    .temp.3 <- NULL;
    h.template <- function() {
      .temp.1 <- .temp.2;
      .temp.3
    }
    .temp.1.name = as.name(".temp.1");
    .temp.2.name = as.name(".temp.2");
    .temp.3.name = as.name(".temp.3");

    # Now we try to adapt the template function to the real task.
    h.body <- pryr::modify_lang(base::body(h.template), function(x) {
      if (base::is.name(x)) {
        if(base::identical(x, .temp.1.name)) {
          return(f2g.as.name);
        }
        if(base::identical(x, .temp.2.name)) {
          return(f.body);
        }
        if(base::identical(x, .temp.3.name)) {
          return(g.body);
        }
      }
      return(x);
    } );
  }

  # So now we have constructed a body and arguments list for h.
  # We now enforce both.
  h.args <- base::force(h.args);
  base::names(h.args) <- base::force(h.args.names);
  h.body <- base::force(h.body);

  # What we need next is an environment for the function.
  if(g.is.primitive) {
    h.env <- base::parent.frame();
  } else {
    h.env <- base::environment(g);
  }
  h.env <- base::force(h.env);

  # We now can create a new function object.
  h <- pryr::make_function(h.args, h.body, h.env)
  h <- base::force(h);

  # Finally, we try to resolve all elements of it.
  h <- .function.canonicalize(f=h, cache=cache);
  h <- base::force(h);
  return(h)
}

