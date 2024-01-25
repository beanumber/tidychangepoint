#' Class for segmented time series
#' @export
#' @param x a numeric vector
#' @examples
#' seg <- tbl_cpt(DataCPSim)
#' str(seg)
#' summarize2(seg)
#' plot(seg)

new_tbl_cpt <- function(x = numeric(), segmenter = NULL, ...) {
  stopifnot(is.numeric(x))

  if (is.null(segmenter)) {
    segmenter <- lm(y ~ 1, data = data.frame(y = x))
  }
  obj <- segmenter |>
    augment() |>
    dplyr::mutate(idx = dplyr::row_number()) |>
    tsibble::as_tsibble(index = idx) |>
    dplyr::group_by(.fitted)
  
  class(obj) <- c("tbl_cpt", class(obj))
  return(obj)
}

#' @rdname new_tbl_cpt
#' @export

validate_tbl_cpt <- function(x) {
  if (!stats::is.ts(stats::as.ts(x$y))) {
    stop("data attribute is not coercible into a ts object.")
  }
  x
}

#' @rdname new_tbl_cpt
#' @export

tbl_cpt <- function(x, ...) {
  obj <- new_tbl_cpt(x, ...)
  validate_tbl_cpt(obj)
}

#' @rdname new_tbl_cpt
#' @export

plot.tbl_cpt <- function(x, ...) {
  regions <- summarize2(x)
  ggplot2::ggplot(
    data = x, 
    ggplot2::aes(x = idx, y = y)
  ) +
    ggplot2::geom_rect(
      data = regions,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf, x = NULL, y = NULL),
      fill = "grey90"
    ) +
    ggplot2::geom_vline(data = regions, ggplot2::aes(xintercept = xmax), linetype = 3) +
    ggplot2::geom_rug(sides = "l") +
    ggplot2::geom_line() +
    ggplot2::geom_smooth(method = "lm", formula = "y ~ 1")
}

#' @rdname new_tbl_cpt
#' @export
summarize2 <- function(.data, ...) UseMethod("summarize2")

#' @rdname new_tbl_cpt
#' @export
summarize2.tbl_cpt <- function(.data, ...) {
  .data |>
    tibble::as_tibble() |>
    dplyr::group_by(.fitted) |>
    dplyr::summarize(
      xmin = min(idx),
      xmax = max(idx),
      ymin = min(y),
      ymax = max(y),
      n = dplyr::n(),
      mean = mean(y),
      sd = sd(y),
      ...
    )
}


#' @rdname new_tbl_cpt
#' @export

print.tbl_cpt <- function(x, ...) {
  NextMethod()
}

#' @rdname new_tbl_cpt
#' @export

summary.tbl_cpt <- function(object, ...) {
  message("List of changepoints object:")
  cat(paste("\nn:"), length(object$data))
  cat(paste("\nBest changepoint set: "))
  cat(cpt_best(object))
  cat(paste("\nNumber of changepoints:"), length(cpt_best(object)))
}

