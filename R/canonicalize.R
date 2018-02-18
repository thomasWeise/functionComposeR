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
#' \code{\link{modify_lang}}, and \code{\link{substitute_q}}   of
#' the package \code{pryr}, the function \code{\link{substituteDirect}}
#' of the package \code{methods} and also of the ideas given by by
#' \code{schloerke} (\url{http://github.com/schloerke}) at
#' \url{http://github.com/hadley/pryr/issues/43}. We also apply
#' \code{\link{expression.simplify}} to take care of the
#' partial resolution of the function body.
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
function.canonicalize <- function(f) {
  f <- base::force(f);
  # If the function is primitive, then it already is "canonical"
  if(base::is.function(f) && (!(base::is.primitive(f)))) {
    f.body <- base::body(f);
    f.body <- base::force(f.body);
    if(is.language(f.body)) {
      # Get the environment of f
      f.env <- base::environment(f);
      f.env <- base::force(f.env);
      if(base::is.null(f.env)) {
        f.env <- .GlobalEnv;
        f.env <- base::force(f.env);
      }
      f.env.ls <- base::ls(envir = f.env)
      f.env.ls <- base::force(f.env.ls);

      # We now try to resolve all promises in f by applying the solution given by
      # schloerke at http://github.com/hadley/pryr/issues/43
      f.body <- pryr::modify_lang(x=f.body,
                                  f=function(x) {
                                    if (base::is.name(x)) {
                                      x.deparsed <- base::deparse(x)
                                      if (x.deparsed %in% f.env.ls) {
                                        # OK, we can resolve the name x
                                        return(base::get(x.deparsed, envir = f.env))
                                      }
                                    }
                                    return(x);
                                  })
      f.body <- base::force(f.body);


      # We now apply the second line of resolution, namely substitute_q
      f.body <- pryr::substitute_q(x=f.body, env=f.env)
      f.body <- base::force(f.body);

      # Now with substitute direct to clean up
      f.body <- methods::substituteDirect(object=f.body, frame=f.env, cleanFunction=TRUE);
      f.body <- base::force(f.body);

      # Now we try to evaluate all evaluate-able sub-expressions of f.
      f.body <- functionComposeR::expression.simplify(f.body, f.env);
      f.body <- base::force(f.body);

      # After the body has been resolved as far is it is possible, we re-compose
      # the function
      f <- pryr::make_function(args=base::formals(f), body=f.body, env=f.env)
      f <- base::force(f);

      # Now we apply the default unenclose method from pryr (for good measures)
      f <- pryr::unenclose(f=f);

      # Finally, we fix the environment
      base::environment(f) <- f.env;
    }
  }

  f <- force(f);
  return(f);
}

