#' @importFrom pryr make_function

#' @include canonicalize.R
#' @include simplify.R

#' @title Substitute Parameters with Values
#'
#' @description Create a copy of a given function where a set of parameters are
#'   replaced with values.
#'
#' @param f the function
#' @param substitutes the arguments to be substituted into the function.
#' @export function.substitute
#' @examples
#' f <- function(x, pars) pars[1] + pars[2]*(x + pars[3]*x)
#' f
#' # function(x, pars) pars[1] + pars[2]*(x + pars[3]*x)
#'  g <- function.substitute(f, list("pars"=c(1, 2, 3)))
#'  g
#' # function (x)
#' #  1 + 2 * (x + 3 * x)
#' h <- function.substitute(f, list("pars"=c(1, 2, 3), "x"=4))
#' # function ()
#' # 33
function.substitute <- function(f, substitutes) {
  .function.substitute (f, substitutes, .cache.make())
}

# The implementation of function.substitute
.function.substitute <- function(f, substitutes, cache) {
  if(is.null(substitutes)) { return(f); }

  substitutes.length <- length(substitutes);
  if(substitutes.length <= 0) { return(f); }

  # We then try to extract the body and arguments of function f
  f.is.primitive <- is.primitive(f);
  if(f.is.primitive) {
    f.args <- formals(args(f));
    f.body <- body(as_closure(f));
    f.env <- new.env();
  } else {
    f.args <- formals(f);
    f.body <- body(f);
    f.env <- environment(f);
  }

  # If the f has no arguments, then the return value of f is irrelevant and we
  # can return f directly.
  if(is.null(f.args)) {
    return(f);
  }

  # Make sure all substitutes are canonical.
  substitutes <- lapply(X=substitutes, FUN=.cache.canonicalize, cache=cache)

  # Turn the substitutes into an environment with the environment of f as
  # parent. This should allow resolving all substitutes as well as all
  # constants.
  temp.env <- as.environment(substitutes);
  parent.env(temp.env) <- f.env;

  # Remember the original body of f.
  f.body.orig <- f.body;

  # Resolve the body, hopefully replacing all occurrences of the parameters with
  # their substitution values.
  f.body <- .expression.simplify(expr=f.body, envir=temp.env, cache=cache);

  # Delete the substituted arguments.
  for(name in names(substitutes)) {
    f.args[name] <- NULL;
  }

  # Compose the new function, with resolved symbols and less arguments.
  if(is.language(f.body)) {
    # After the body has been resolved as far is it is possible, we re-compose
    # the function
    f <- make_function(args=f.args, body=f.body, env=f.env)
    f <- force(f);
    # Now we apply the default unenclose method from pryr (for good measures)
    f <- .function.canonicalize(f=f, cache=cache);
  } else {
    # If we get here, the body of f somehow became a constant.
    # This won't fly with make_function nor with unenclose.
    # So we have to first construct a function with the original body,
    # then change the body to the new (constant) body.
    f <- make_function(args=f.args, body=f.body.orig, env=f.env)
    body(f) <- f.body;
  }

  # Finally, we  enforce f and are finished
  f <- force(f);
  return(f)
}

