#' @importFrom pryr modify_lang make_function
#' @importFrom stringr str_count str_detect
#' @importFrom rlang as_closure

#' @include canonicalize.R

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
function.compose <- function(f, g, f2g="x") {

  # First, we canonicalize g
  g <- functionComposeR::function.canonicalize(g);
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
  f <- functionComposeR::function.canonicalize(f);
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
  f.discovery <- stringr::str_count(f2g, f.args.names);
  if(base::sum(f.discovery) <= 0) {
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
  for(i in 1:length(f.args)) {
   f.arg.name <- f.args.names[[i]];
    if(sum(stringr::str_detect(f.arg.name, h.args.names)) <= 0) {
      # We add the argument only if it is not part of the argument list of h
      h.args[[length(h.args)+1]] <- f.args[[i]];
      h.args.names <- c(h.args.names, f.arg.name);
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
    h.env = base::parent.frame();
  } else {
    h.env <- base::environment(g);
  }
  h.env <- base::force(h.env);

  # We now can create a new function object.
  h <- pryr::make_function(h.args, h.body, h.env)
  h <- base::force(h);

  # Finally, we try to resolve all elements of it.
  h <- functionComposeR::function.canonicalize(h);
  h <- base::force(h);
  return(h)
}

