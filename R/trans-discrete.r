#' Flip transformation.
#'
#' @export
flip_trans <- function() {
  trans_new("flip",
            function(x) {r <- factor(x); factor(r, levels = rev(levels(r)))},
            function(x) {r <- factor(x); factor(r, levels = rev(levels(r)))})
}

