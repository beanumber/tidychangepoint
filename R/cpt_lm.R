#' Methods for `cpt_lm`
#' @export
#' @examples
#' cpts <- segment(DataCPSim)
#' as.ts(cpts)
#' changepoints(cpts)

as.ts.cpt_lm <- function(x, ...) {
  as.ts(x$model$y)
}

#' @rdname as.ts.cpt_lm
#' @export
changepoints.cpt_lm <- function(x, ...) {
  x$contrasts |>
    names() |>
    sub(pattern = "t > ", replacement = "") |>
    as.integer()
}
