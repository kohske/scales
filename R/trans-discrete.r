#' Flip transformation.
#'
#' @export
flip_trans <- function() {
  f <- function(x) {
    if (is.numeric(x)) {
      -x
    } else if (is.character(x)) {
      x <- factor(x)
      factor(x, levels = rev(levels(x)))
    } else if (is.factor(x)) {
      factor(x, levels = rev(levels(x)))
    } else {
      # NA, NULL, etc.
      x
    }
  }
  trans_new("flip", f, f)
}

