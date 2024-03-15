#' Compatibility layer for `lm`
#' @param x An `lm` object
#' @param ... arguments passed to methods
#' @export
#' @examples
#' cpts <- segment(DataCPSim)
#' as.ts(cpts)
#' changepoints(cpts)

as.ts.lm <- function(x, ...) {
  as.ts(x$model$y)
}

#' @rdname as.ts.lm
#' @export
changepoints.lm <- function(x, ...) {
  x$contrasts |>
    names() |>
    sub(pattern = "t > ", replacement = "") |>
    as.integer()
}
