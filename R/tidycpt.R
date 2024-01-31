#' Segment a time series using a variety of algorithms
#' @param x a numeric vector coercible into a [stats::ts] object
#' @param method a character string indicating the algorithm to use
#' @param ... arguments passed to methods
#' @return an object of class `changepoint::cpt`, [stats::lm], or [cpt_gbmdl]
#' @seealso [changepoint::cpt.meanvar()]
#' @export
#' @examples
#' mod_null <- segment(DataCPSim)
#' augment(mod_null)
#' tidy(mod_null)
#' glance(mod_null)

segment <- function(x, method = "null", ...) UseMethod("segment")

#' @rdname segment
#' @export

segment.numeric <- function(x, method = "null", ...) {
  if (!stats::is.ts(stats::as.ts(x))) {
    stop("x is not coercible into a ts object.")
  }
  segment(as.ts(x), method = method, ... = ...)
}

#' @rdname segment
#' @export
#' @examples
#' segment(DataCPSim, method = "cpt-pelt")
#' segment(DataCPSim, method = "cpt-pelt", penalty = "AIC")
#' segment(DataCPSim, method = "cpt-binseg", penalty = "AIC")
#' segment(DataCPSim, method = "cpt-segneigh", penalty = "BIC")
#' segment(DataCPSim, method = "cpt-manual", cpts = c(826))
#' two_cpts <- segment(DataCPSim, method = "cpt-manual", cpts = c(365, 826))
#' plot(two_cpts)
#' \dontrun{
#' segment(DataCPSim, method = "cpt-gbmdl")
#' }
#' 

segment.ts <- function(x, method = "null", ...) {
  message(paste("method:", method))
  n <- length(x)
  ds <- data.frame(y = x, t = 1:n)
  if (method == "cpt-pelt") {
    mod <- changepoint::cpt.meanvar(data = x, method = "PELT", ...)
  }
  if (method == "cpt-binseg") {
    mod <- changepoint::cpt.meanvar(data = x, method = "BinSeg", ...)
  }
  if (method == "cpt-segneigh") {
    mod <- changepoint::cpt.meanvar(data = x, method = "SegNeigh", ...)
  }
  if (method == "cpt-gbmdl") {
    mod <- segment_gbmdl(x, param = param, ...)
  }
  if (method == "cpt-manual") {
    message("\nSegmenting using manually input changepoints...")
    args <- list(...)
    cpts <- args[["cpts"]]
    terms <- paste("(t > ", cpts, ")") |>
      paste(collapse = "+")
    form <- stats::as.formula(paste("y ~ ", terms))
    mod <- stats::lm(form, data = ds)
    class(mod) <- c("cpt_lm", class(mod))
  }
  if (method == "null") {
    mod <- stats::lm(y ~ 1, data = ds)
    class(mod) <- c("cpt_lm", class(mod))
  }
  # build the tidycpt object
  obj <- list(segmenter = mod)
  class(obj) <- c("tidycpt")
  return(obj)
}

#' @rdname segment
#' @export
as.ts.tidycpt <- function(x, ...) {
  as.ts(x$segmenter)
}

#' @rdname segment
#' @export
changepoints <- function(x, ...) UseMethod("changepoints")

#' @rdname segment
#' @export
changepoints.tidycpt <- function(x, ...) {
  changepoints(x$segmenter)
}

#' @rdname segment
#' @export
augment.tidycpt <- function(x, ...) {
  augment(x$segmenter)
}

#' @rdname segment
#' @export
tidy.tidycpt <- function(x, ...) {
  augment(x) |>
    dplyr::ungroup() |>
    # why is this necessary????
    as.data.frame() |>
    dplyr::group_by(region) |>
    dplyr::summarize(
      num_obs = dplyr::n(),
      left = min(index),
      right = max(index),
      min = min(y),
      max = max(y),
      mean = mean(y),
      sd = sd(y),
      ... = ...
    )
}

#' @rdname segment
#' @export
glance.tidycpt <- function(x, ...) {
  glance(x$segmenter)
}

#' @rdname segment
#' @export
regions <- function(x, ...) UseMethod("regions")

#' @rdname segment
#' @export
regions.tidycpt <- function(x, ...) {
  tidy(x) |>
    dplyr::pull(region) |> 
    levels()
}


#' @rdname segment
#' @export

plot.tidycpt <- function(x, ...) {
  regions <- tidy(x)
  ggplot2::ggplot(
    data = augment(x), 
    ggplot2::aes(x = index, y = y)
  ) +
    ggplot2::geom_rect(
      data = regions,
      ggplot2::aes(xmin = left, xmax = right, ymin = 0, ymax = Inf, x = NULL, y = NULL),
      fill = "grey90"
    ) +
    ggplot2::geom_vline(data = regions, ggplot2::aes(xintercept = right), linetype = 3) +
    ggplot2::geom_rug(sides = "l") +
    ggplot2::geom_line() + 
    ggplot2::geom_segment(
      data = regions,
      ggplot2::aes(x = left, y = mean, xend = right, yend = mean),
      color = "red"
    ) +
    ggplot2::geom_segment(
      data = regions,
      ggplot2::aes(x = left, y = mean + 1.96 * sd, xend = right, yend = mean + 1.96 * sd),
      color = "red",
      linetype = 3
    ) +
    ggplot2::geom_segment(
      data = regions,
      ggplot2::aes(x = left, y = mean - 1.96 * sd, xend = right, yend = mean - 1.96 * sd),
      color = "red",
      linetype = 3
    )
}