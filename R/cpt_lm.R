#' Methods for `cpt_lm`
#' @export
augment.cpt_lm <- function(x, ...) {
  out <- NextMethod()
  tau <- changepoints(x)
  tau_padded <- c(0, tau, length(as.ts(x)))
  out |>
    dplyr::mutate(index = dplyr::row_number()) |>
    tsibble::as_tsibble(index = index) |>
    dplyr::mutate(region = cut(
      index, 
      breaks = tau_padded, 
      include.lowest = TRUE, 
      right = FALSE)) |>
    dplyr::group_by(region)
}

#' @rdname augment.cpt_lm
#' @export
#' @examples
#' cpts <- segment(DataCPSim)
#' as.ts(cpts)

as.ts.cpt_lm <- function(x, ...) {
  as.ts(x$model$y)
}

#' @rdname augment.cpt_lm
#' @export
changepoints.cpt_lm <- function(x, ...) {
  x$contrasts |>
    names() |>
    sub(pattern = "t > ", replacement = "") |>
    as.integer()
}
