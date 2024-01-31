#' Methods for `cpt_lm`
#' @export
augment.cpt_lm <- function(x, ...) {
  augment(x, ...) |>
    dplyr::mutate(index = dplyr::row_number()) |>
    tsibble::as_tsibble(index = index) |>
    dplyr::mutate(region = cut(
      index, 
      breaks = c(0, length(x$fitted.values)), 
      include.lowest = TRUE, 
      right = FALSE)) |>
    dplyr::group_by(region)
}

#' @rdname augment
#' @export
#' @examples
#' cpts <- segment(DataCPSim)
#' as.ts(cpts)

as.ts.cpt_lm <- function(x, ...) {
  message("\nDispatching on segmented linear model object...")
  as.ts(x$model$y)
}

