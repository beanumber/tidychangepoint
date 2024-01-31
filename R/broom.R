#' Broom compatibility layer
#' @export
#' @importFrom broom tidy
#' @seealso [broom::tidy()]
tidy <- broom::tidy

#' @rdname tidy
#' @export
#' @importFrom broom augment
#' @seealso [broom::augment()]
augment <- broom::augment

#' @rdname augment
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

#' @rdname tidy
#' @export
#' @importFrom broom glance
#' @seealso [broom::glance()]
glance <- broom::glance
