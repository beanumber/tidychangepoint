#' Segment a time series using a variety of algorithms
#' 
#' @description
#' A wrapper function that encapsulates various algorithms for detecting changepoint
#' sets in univariate time series. 
#' 
#' @param x a numeric vector coercible into a [stats::ts] object
#' @param method a character string indicating the algorithm to use. See Details.
#' @param ... arguments passed to methods
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
#' @return an object of class `tidycpt`. Every `tidycpt` object contains:
#' - `segmenter`: The object returned by the underlying function. 
#' - `nhpp`: A `tbl_df` showing the best fit parameters for each region, as 
#'   defined by the chnagepoint set returned by `changepoints()`. These parameters
#'   are fit using the non-homogeneous Poisson process model in [fit_nhpp()].
#' @details Currently, [segment()] can use the following algorithms, depending
#' on the value of the `method` argument:
#' - `cpt-pelt`: Uses the PELT algorithm as implemented in 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `cpt-binseg`: Uses the Binary Segmentation algorithm as implemented by 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `cpt-segneigh`: Uses the Segmented Neighborhood algorithm as implemented by 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `single-best`: Uses the AMOC criteria as implemented by 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `cpt-gbmdl`: Uses the Genetic BMDL heuristic as implemented by 
#'   [segment_gbmdl()]. The `segmenter` is of class [cpt-gbmdl].
#' - `cpt-manual`: Uses the vector of changepoints in the `cpts` arugment and
#'   [stats::lm()]. The `segmenter` is of class `lm`.
#' - `null`: The default. Uses [stats::lm()] with no changepoints. 
#'   The `segmenter` is of class `lm`.
#' [stats::lm], or [cpt_gbmdl]
#' @seealso [changepoint::cpt.meanvar()]
#' @examples
#' segment(DataCPSim, method = "cpt-pelt")
#' segment(DataCPSim, method = "cpt-pelt", penalty = "AIC")
#' segment(DataCPSim, method = "cpt-binseg", penalty = "AIC")
#' segment(DataCPSim, method = "cpt-segneigh", penalty = "BIC")
#' segment(DataCPSim, method = "cpt-manual", cpts = c(826))
#' two_cpts <- segment(DataCPSim, method = "cpt-manual", cpts = c(365, 826))
#' plot(two_cpts)
#' diagnose(two_cpts)
#' \dontrun{
#' x <- segment(DataCPSim, method = "cpt-gbmdl")
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
  if (method == "single-best") {
    mod <- changepoint::cpt.meanvar(data = x, method = "AMOC", ...)
  }
  if (method == "cpt-gbmdl") {
    mod <- segment_gbmdl(x, ...)
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
  obj <- list(
    segmenter = mod,
    nhpp = fit_nhpp(as.ts(x), changepoints(mod))
  )
  class(obj) <- c("tidycpt")
  return(obj)
}

#' Generic functions for tidycpt objects
#' 
#' @param x A `tidycpt` object
#' @param ... arguments passed to methods
#' @export
changepoints <- function(x, ...) UseMethod("changepoints")

#' @rdname changepoints
#' @export
changepoints.tidycpt <- function(x, ...) {
  changepoints(x$segmenter)
}

#' @rdname changepoints
#' @export
as.ts.tidycpt <- function(x, ...) {
  as.ts(x$segmenter)
}

#' @rdname changepoints
#' @export
length.tidycpt <- function(x, ...) {
  length(as.ts(x))
}

#' @rdname changepoints
#' @param object A `tidycpt` object
#' @seealso [stats::logLik()]
#' @export
logLik.tidycpt <- function(object, ...) {
  logLik(object$segmenter)
}

#' @rdname changepoints
#' @export
MBIC <- function(object, ...) UseMethod("MBIC")

#' @rdname changepoints
#' @references Zhang and Seigmmund (2007) for MBIC: \doi{10.1111/j.1541-0420.2006.00662.x}
#' @export
MBIC.tidycpt <- function(object, ...) {
  tau <- changepoints(object)
  m <- length(tau)
  r <- tau / length(object)
  -(1/2) * (3 * m * log(length(object)) + sum(r)) 
}

#' @rdname changepoints
#' @seealso [stats::nobs()]
#' @export
nobs.tidycpt <- function(object, ...) {
  nobs(object$segmenter)
}

#' @rdname changepoints
#' @seealso [broom::augment()]
#' @export
augment.tidycpt <- function(x, ...) {
  tau <- changepoints(x)
  tibble::enframe(as.ts(x), name = "index", value = "y") |>
    tsibble::as_tsibble(index = index) |>
    dplyr::mutate(
      region = cut_inclusive(index, pad_tau(tau, length(x)))
    ) |>
    dplyr::group_by(region)
}


#' @rdname changepoints
#' @seealso [broom::tidy()]
#' @export
tidy.tidycpt <- function(x, ...) {
  tau <- changepoints(x)
  theta <- x$nhpp
  n <- length(x)
  
  augment(x) |>
    dplyr::ungroup() |>
    # why is this necessary????
    as.data.frame() |>
    dplyr::group_by(region) |>
    dplyr::summarize(
      num_obs = dplyr::n(),
#      left = min(index),
#      right = max(index),
      min = min(y),
      max = max(y),
      mean = mean(y),
      sd = stats::sd(y),
      ... = ...
    ) |>
    dplyr::inner_join(theta, by = "region")
}

#' @rdname changepoints
#' @seealso [broom::glance()]
#' @export
glance.tidycpt <- function(x, ...) {
  glance(x$segmenter) |>
    dplyr::mutate(
      s3_logLik = logLik(x),
      s3_AIC = AIC(x),
      s3_BIC = BIC(x),
      s3_MBIC = MBIC(x),
      s3_BMDL = bmdl(x, changepoints(x))
    )
}

#' @rdname changepoints
#' @export
regions <- function(x, ...) UseMethod("regions")

#' @rdname changepoints
#' @export
regions.tidycpt <- function(x, ...) {
  x$nhpp |>
    dplyr::pull(region) |> 
    levels()
}

#' @rdname changepoints
#' @export

plot.tidycpt <- function(x, ...) {
  regions <- tidy(x)
  ggplot2::ggplot(
    data = augment(x), 
    ggplot2::aes(x = index, y = y)
  ) +
    ggplot2::geom_rect(
      data = regions,
      ggplot2::aes(xmin = begin, xmax = end, ymin = 0, ymax = Inf, x = NULL, y = NULL),
      fill = "grey90"
    ) +
    ggplot2::geom_vline(data = regions, ggplot2::aes(xintercept = end), linetype = 3) +
    ggplot2::geom_rug(sides = "l") +
    ggplot2::geom_line() + 
    ggplot2::geom_segment(
      data = regions,
      ggplot2::aes(x = begin, y = mean, xend = end, yend = mean),
      color = "red"
    ) +
    ggplot2::geom_segment(
      data = regions,
      ggplot2::aes(x = begin, y = mean + 1.96 * sd, xend = end, yend = mean + 1.96 * sd),
      color = "red",
      linetype = 3
    ) +
    ggplot2::geom_segment(
      data = regions,
      ggplot2::aes(x = begin, y = mean - 1.96 * sd, xend = end, yend = mean - 1.96 * sd),
      color = "red",
      linetype = 3
    ) + 
    ggplot2::scale_x_continuous("Time Index (t)") +
    ggplot2::scale_y_continuous("Original Measurement")
}


#' @rdname changepoints
#' @export
#' @examples
#' plot_mcdf(segment(DataCPSim))

plot_mcdf <- function(x, ...) {
  tau <- changepoints(x)
  theta <- tidy(x)
  n <- length(x)
  
  z <- exceedances(x) |>
    tibble::enframe(name = "cum_exceedances", value = "t_exceedance") |>
    # always add the last observation
    dplyr::bind_rows(
      data.frame(
        cum_exceedances = c(0, length(exceedances(x))), 
        t_exceedance = c(0, n)
      )
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      m = cdf_exceedances_est(t_exceedance, tau = tau, theta = theta, n = n),
      lower = stats::qpois(0.05, lambda = m),
      upper = stats::qpois(0.95, lambda = m),
    )
  
  regions <- tidy(x)
  ggplot2::ggplot(data = z, ggplot2::aes(x = t_exceedance, y = cum_exceedances)) +
    ggplot2::geom_vline(data = regions, ggplot2::aes(xintercept = end), linetype = 3) +
    ggplot2::geom_abline(intercept = 0, slope = 0.5, linetype = 3) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Time Index (t)", limits = c(0, n)) +
    ggplot2::scale_y_continuous("Cumulative Number of Exceedances (N)") +
    ggplot2::geom_line(ggplot2::aes(y = m), color = "red") +
    ggplot2::geom_line(ggplot2::aes(y = lower), color = "blue") +
    ggplot2::geom_line(ggplot2::aes(y = upper), color = "blue")
}

#' @rdname changepoints
#' @export
diagnose <- function(x, ...) UseMethod("diagnose")

#' @rdname changepoints
#' @export
#' @examples
#' diagnose(segment(DataCPSim))
#' diagnose(segment(DataCPSim, method = "single-best"))
#' diagnose(segment(DataCPSim, method = "cpt-pelt"))
#' diagnose(segment(test_set()))
#' diagnose(segment(test_set(n = 2, sd = 4), method = "cpt-pelt"))
#' 
diagnose.tidycpt <- function(x, ...) {
  patchwork::wrap_plots(
    plot(x),
    plot_mcdf(x),
    ncol = 1
  )
}
