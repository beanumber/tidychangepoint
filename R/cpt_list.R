#' Class for candidate changepoints
#' @export
#' @param x a numeric vector
#' @examples
#' cpts <- cpt_list(rlnorm_ts_1)
#' str(cpts)
#' plot(cpts)

new_cpt_list <- function(x = numeric()) {
  stopifnot(is.numeric(x))
  structure(
    list(
      data = as.ts(x),
      n = length(data)
    ), 
    class = "cpt_list"
  )
}

#' @rdname new_cpt_list
#' @export

validate_cpt_list <- function(x) {
  if (!is.ts(x$data)) {
    stop("data attribute is not coercible into a ts object.")
  }
  x
}

#' @rdname new_cpt_list
#' @export

cpt_list <- function(x) {
  obj <- new_cpt_list(x)
  validate_cpt_list(obj)
}

#' @rdname new_cpt_list
#' @export

plot.cpt_list <- function(x, ...) {
  plot(x$data)
}
