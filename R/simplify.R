# Ignores all of its arguments
.ignore <- function(x) {}

# a constant for the enclosure begin
.enclosure.begin <- as.name("{")
# a constant for the assignment begin
.assignment.begin <- as.name("<-")

# make a cache
.cache.make <- function() {
  return(new.env(hash=FALSE));
}

# Canonicalize an expression by abusing an environment as modifiable list
.cache.canonicalize <- function(expr, cache) {
  .cache.list <- as.list(cache);
  for (.value in .cache.list) {
    if(identical(.value, expr)) {
      return(.value);
    }
  }
  cache[[toString(length(.cache.list)+1)]] <- expr;
  return(expr);
}


#' @title Recursively Simplify an Expression in a given Environment
#' @description We try to recursivelys simplify the expression \code{expr}
#' by trying to evaluate all of its sub-expressions. A sub-expression
#' that has been successfully evaluated is replaced with the evaluation
#' result.
#'
#' \code{expression.simplify} is somewhat similar to \code{\link{eval}},
#' with the difference that it can deal with expressions which are only
#' partially evaluatable. The body of the function \code{function(x) { 5+4+x }},
#' for instance, cannot be fully evaluated if the value of \code{x} is unknown.
#' We can, however, replace the \code{5+4} with a \code{9}.
#'
#' Since this function is still an early draft, there may be many scenarios
#' where it does not simplify an expression as much as it could be possible.
#' Hopefully, we can impove this in the future and make it more powerful.
#'
#' Additionally, this function makes sure that identical sub-expressions will be
#' replaced by the exactly same object. This may have advantages, e.g., making
#' the expression more cache friendly when the object is a vector.
#'
#' @param expr the expression to be simplified
#' @param envir the environment in which the expression is to be simplified
#' @export expression.simplify
#' @examples
#' expression.simplify(5+3)
#' # [1] 8
#' k <- 8
#' expression.simplify( ((5+3*k)-1)/k - 2^tan(k) )
#' # [1] 3.491024
#' f <- function(x) { 5+3+x }
#' expression.simplify(body(f), envir=environment(f))
#' # 8 + x
expression.simplify <- function(expr, envir) {
  .expression.simplify(expr=expr, envir=envir, cache=.cache.make());
}

# The implementation of expression.simplify
#' @importFrom utilizeR ignoreErrors
.expression.simplify <- function(expr, envir, cache) {
  # If the expression is already numeric, we don't need to do anything
  if(is.numeric(expr)) {
    return(.cache.canonicalize(expr=expr, cache=cache));
  }

  # Convert the expression to a list.
  expr.list <- as.list(expr);
  expr.length <- length(expr.list);

  # Is the expression an assignment?
  expr.is.assignment = ((expr.length >= 3) && (identical(expr.list[[1]], .assignment.begin)));

  # We only consider evaluating the expression if it is not an assignment.
  # Otherwise we pollute the environment.
  if(!(expr.is.assignment)) {
    # First, we try to directly evaluate the expression
    if(is.language(expr)) {
      # Try to evaluate the expression.
      ignoreErrors({
        # If the expression contains an assignment, evaluating it in its parent
        # environment will pollute this enviroment (by creating or modifying the
        # variable assigned to). In order to prevent this or similar effects, we
        # evaluate the expression in a child-environment envir.cpy.
        envir.cpy <- new.env(parent=envir);
        result <- eval(expr=expr, envir=envir.cpy);
        result <- force(result);
        if(!(is.primitive(result))) {
          # Cool, everything has worked and the expression is evaluated.
          # We can replace it with its result
          return(.cache.canonicalize(expr=result, cache=cache));
        }
      })
    }
  }

  # If the list has length 1, we can return it directly.
  if((expr.length <= 1)) {
    return(.cache.canonicalize(expr=expr, cache=cache));
  }

  # If there are missing elements (this may happen if there is a `...` involved somewhere)
  # then we better not touch them...
  for(element in expr.list) {
    if(missing(element)) {
      # If there are missing values, we are done and quit immediately
      return(.cache.canonicalize(expr=expr, cache=cache));
    }
  }

  # We try to remove useless enclosures such as "{x}" by converting them to the
  # form "x".
  if((expr.length == 2) && identical(expr.list[[1]], .enclosure.begin)) {
    result <- .expression.simplify(expr=expr.list[[2]],
                                   envir=envir,
                                   cache=cache);
    result <- force(result);
    return(result);
  }

  # Otherwise, we will recursively try to evaluate all sub-expressions
  # and return the result.
  if(expr.is.assignment){
    # Of course, if the expression is an assignment, we won't destroy it by resolving
    # the target parameter name.
    result.list <- lapply(X=1:expr.length,
                                FUN=function(x, envir, cache) {
                                  if(x <= 2) { return(expr.list[[x]]); }
                                  .expression.simplify(expr.list[[x]], envir, cache)
                                },
                                envir=envir,
                                cache=cache);
  } else {
    result.list <- lapply(X=expr.list,
                                FUN=.expression.simplify,
                                envir=envir,
                                cache=cache);
  }
  result <- .cache.canonicalize(expr=as.call(result.list), cache=cache);
  result <- force(result);
  return(result);
}
