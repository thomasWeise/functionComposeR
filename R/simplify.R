# Ignores all of its arguments
.ignore <- function(x) {}

# a constant for the enclosure begin
.enclosure.begin <- base::as.name("{")

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
  if(base::is.language(expr)) {
    # Try to evaluate the expression.
    tryCatch({
      result <- base::eval(expr=expr, envir=envir);
      result <- base::force(result);
      if(!(base::is.primitive(result))) {
        # Cool, everything has worked and the expression is evaluated.
        # We can replace it with its result
        return(result);
      }
    }, error=.ignore, warning=.ignore)
  }
  # Convert the expression to a list.
  expr.list <- base::as.list(expr);

  expr.length <- base::length(expr.list);
  # If the list has length 1, we can return it directly.
  if(expr.length <= 1) { return(expr); }
  # We try to remove useless enclosures such as "{x}" by converting them to the
  # form "x".
  if((expr.length == 2) && base::identical(expr.list[[1]], .enclosure.begin)) {
    result <- expression.simplify(expr.list[[2]], envir);
    result <- force(result);
    return(result);
  }

  # Otherwise, we will recursivel try to evaluate all sub-expressions
  # and return the result.
  result <- base::as.call(base::lapply(X=expr.list, FUN=expression.simplify, envir=envir));
  result <- base::force(result);
  return(result);
}