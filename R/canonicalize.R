#' @importFrom pryr modify_lang substitute_q make_function unenclose
#' @importFrom methods substituteDirect
#' @include simplify.R

#' @title Canonicalize a Function
#'
#' @description \code{function.canonicalize(f)} tries to generate a canonical and
#' compact version of the function \code{f}. It therefore tries to substitute
#' all variables in the body of \code{f} with their value if the value is known
#' and then tries to evaluate all sub-expressions whose results will be
#' constant. In other words, the promises, variables, and expressions inside a
#' function \code{f} are evaluated and replaced with constant values where
#' possible.
#'
#' We make use of the functions \code{\link{unenclose}},
#' \code{\link{modify_lang}}, and \code{\link{substitute_q}} of the package
#' \code{pryr}, the function \code{\link{substituteDirect}} of the package
#' \code{methods} and also of our recursive version of the ideas given by by
#' \code{schloerke} (\url{http://github.com/schloerke}) at
#' \url{http://github.com/hadley/pryr/issues/43}. We also apply
#' \code{\link{expression.simplify}} to take care of the partial resolution of
#' the function body and for replacing identical sub-expressions with the same
#' object (e.g., to make the execution more cache friendly).
#'
#' These measures are intented to produce a version of a function \code{f} which
#' is both human-readable and as fast to evaluate as possible. The new function
#' will reside in the same environment as \code{f}.
#'
#' @param f the function
#' @return the canonicalized function
#'
#' @export function.canonicalize
#'
#' @examples
#' f <- function(x) { 5+3+x }
#' function.canonicalize(f)
#' # function (x)
#' # 8 + x
#' z <- 24;
#' g <- function(x) { tan(sin(z) + (z*27) / x) }
#' function.canonicalize(g)
#' # function (x)
#' # tan(-0.905578362006624 + 648/x)
#' a <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)
#' b <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)
#' c <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)
#' f <- function(x) (c-(x*a)/b)*(a+b+c)
#' g <- function.canonicalize(f)
#' g
#' # function (x)
#' # (c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
#' # 18, 19, 20, 21, 22, 23, 24, 25) - (x * c(1, 2, 3, 4, 5, 6, 7,
#' # 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
#' # 24, 25))/c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
#' # 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)) * c(3, 6, 9, 12, 15,
#' # 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63,
#' # 66, 69, 72, 75)
#' library(microbenchmark)
#' x <- runif(1000)
#' microbenchmark(f(x), g(x))
#' # Unit: microseconds
#' # expr   min    lq     mean median     uq    max neval
#' # f(x) 8.868 9.097 10.01950 9.2935 9.4805 35.340   100
#' # g(x) 8.285 8.506  9.38347 8.6730 8.7650 28.501   100
function.canonicalize <- function(f) {
  .function.canonicalize(f, .cache.make())
}

# The implementation of function.canonicalize
.function.canonicalize <- function(f, cache) {
  f <- force(f);
  # If the function is primitive, then it already is "canonical"
  if(is.function(f) && (!(is.primitive(f)))) {
    f.body <- body(f);
    f.body <- force(f.body);

    # Only if the function body is a language element, we can optimize it.
    # If is.language(f.body) is FALSE, we probably have a constant
    # function here, so we don't need (and neither can) improve it.
    if(is.language(f.body)) {
      f.body.orig <- f.body;

      # Get the environment of f
      f.env <- environment(f);
      f.env <- force(f.env);
      if(is.null(f.env)) {
        f.env <- .GlobalEnv;
        f.env <- force(f.env);
      }
      f.env.ls <- ls(envir = f.env)
      f.env.ls <- force(f.env.ls);

      # We now try to resolve all promises in f by applying the solution given by
      # schloerke at http://github.com/hadley/pryr/issues/43
      f.body <- pryr::modify_lang(x=f.body,
                    f=function(x) {
                      if (is.name(x)) {
                        x.deparsed <- deparse(x)
                        if (x.deparsed %in% f.env.ls) {
                          # OK, we can resolve the name x
                          replacement <- get(x.deparsed, envir = f.env);
                          if(is.function(replacement)) {
                            # If we make a function call, try to
                            # recursively canonicalize that call
                            # (different from the schloerke solution)
                            if(!(identical(f, replacement))) {
                              return(.function.canonicalize(f=replacement,
                                                            cache=cache));
                            }
                          }
                          return(replacement);
                        }
                      }
                      return(x);
                    })
      f.body <- force(f.body);

      if(is.language(f.body)) {
        # We now apply the second line of resolution, namely substitute_q,
        # but only if f.body has not become a constant yet.
        f.body <- pryr::substitute_q(x=f.body, env=f.env)
        f.body <- force(f.body);
      }

      # Now with substitute direct to clean up
      f.body <- methods::substituteDirect(object=f.body, frame=f.env, cleanFunction=TRUE);
      f.body <- force(f.body);

      # Now we try to evaluate all evaluate-able sub-expressions of f.
      f.body <- .expression.simplify(expr=f.body, envir=f.env, cache=cache);
      f.body <- force(f.body);

      if(is.language(f.body)) {
        # After the body has been resolved as far is it is possible, we re-compose
        # the function
        f <- pryr::make_function(args=formals(f), body=f.body, env=f.env)
        f <- force(f);
        # Now we apply the default unenclose method from pryr (for good measures)
        f <- pryr::unenclose(f=f);
      } else {
        # If we get here, the body of f somehow became a constant.
        # This won't fly with pryr::make_function nor with pryr::unenclose.
        # So we have to first construct a function with the original body,
        # then change the body to the new (constant) body.
        f <- pryr::make_function(args=formals(f), body=f.body.orig, env=f.env)
        body(f) <- f.body;
      }
      f <- force(f);

      # Finally, we fix the environment
      environment(f) <- f.env;
    }
  }

  f <- force(f);
  return(f);
}

