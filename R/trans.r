#' Create a new transformation object.
#'
#' A transformation encapsulates a transformation and its inverse, as well
#' as the information needed to create pleasing breaks and labels. The breaks
#' function is applied on the transformed range of the range, and it's
#' expected that the labels function will perform some kind of inverse
#' tranformation on these breaks to give them labels that are meaningful on 
#' the original scale.
#'
#' @param name transformation name
#' @param transform function, or name of function, that performs the
#    transformation
#' @param inverse function, or name of function, that performs the
#    inverse of the transformation
#' @param breaks default breaks function for this transformation. The breaks
#'   function is applied to the raw data.
#' @param format default format for this transformation. The format is applied
#'   to breaks generated to the raw data.
#' @seealso \Sexpr[results=rd]{scales:::seealso_trans()}
#' @export trans_new is.trans
#' @aliases trans_new trans is.trans
#' @S3method print trans
trans_new <- function(name, transform, inverse, breaks = pretty_breaks(), format = scientific_format()) {
  if (is.character(transform)) transform <- match.fun(transform)
  if (is.character(inverse)) inverse <- match.fun(inverse)
  
  structure(list(name = name, transform = transform, inverse = inverse,
    breaks = breaks, format = format), 
    class = "trans")
}

is.trans <- function(x) inherits(x, "trans")
print.trans <- function(x, ...) cat("Transformer: ", x$name)

#' Convert character string to transformer.
#'
#' @param x name of transformer
#' @export
as.trans <- function(x) {
  if (is.trans(x)) return(x)
  
  f <- str_c(x, "_trans")
  match.fun(f)()
}

#' Chain multiple transformer.
#'
#' @param ... transformers
#' @param breaks breaks function for this transformation.
#' @param format format function for this transformation.
#' @seealso \Sexpr[results=rd]{scales:::trans_new()}
#' @export
#' @examples
#' trs <- trans_chain("log","reverse")
#' trs$trans(1:5)
#' trs$inverse(-(log(1:5)))
trans_chain <- function(..., breaks = pretty_breaks(), format = scientific_format()) {
  trs <- Map(as.trans, list(...))
  Funcall <- function(f, ...) f(...)
  trans_new(name = str_c(sapply(trs, function(x)x$name), collapse="->"),
    transform = function(x) Reduce(Funcall, sapply(rev(trs), function(y)y$transform), x, right=TRUE),
    inverse = function(x) Reduce(Funcall, sapply(trs, function(y)y$inverse), x, right=TRUE),
    breaks = breaks,
    format = format)
}
