#' Segment a time series using a variety of algorithms
#' 
#' @description
#' A wrapper function that encapsulates various algorithms for detecting changepoint
#' sets in univariate time series. 
#' 
#' @param x a [tsibble::tsibble] or numeric vector coercible into a [stats::ts] object
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
segment.tbl_ts <- function(x, method = "null", ...) {
  if (!stats::is.ts(stats::as.ts(x))) {
    stop("x is not coercible into a ts object.")
  }
  segment(as.ts(x), method = method, ... = ...)
}

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
#'   This object also has class `nhpp`
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
#'   [segment_gbmdl()]. The `segmenter` is of class [cpt_gbmdl].
#' - `manual`: Uses the vector of changepoints in the `cpts` arugment and
#'   [stats::lm()]. The `segmenter` is of class `seg_default`.
#' - `null`: The default. Uses [stats::lm()] with no changepoints. 
#'   The `segmenter` is of class `seg_default`.
#' [stats::lm], or [cpt_gbmdl]
#' @seealso [changepoint::cpt.meanvar()]
#' @examples
#' segment(DataCPSim, method = "cpt-pelt")
#' segment(DataCPSim, method = "cpt-pelt", penalty = "AIC")
#' segment(DataCPSim, method = "cpt-binseg", penalty = "AIC")
#' segment(DataCPSim, method = "cpt-segneigh", penalty = "BIC")
#' segment(DataCPSim, method = "random", num_generations = 10)
#' segment(DataCPSim, method = "manual", cpts = c(826))
#' two_cpts <- segment(DataCPSim, method = "manual", cpts = c(365, 826))
#' plot(two_cpts)
#' diagnose(two_cpts)
#' \dontrun{
#' x <- segment(DataCPSim, method = "cpt-gbmdl")
#' }
#' 

segment.ts <- function(x, method = "null", ...) {
  args <- list(...)
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
  if (method == "random") {   
    if("num_generations" %in% names(args)) {
      num_generations <- args[["num_generations"]]
    } else {
      num_generations <- 50
    }
    cpts <- random_cpts(x, n = num_generations)
    mod <- new_seg_default(x, cpt_list = cpts)
  }
  if (method == "manual") {
    if(!"cpts" %in% names(args)) {
      stop("Please supply the cpts argument to use the manual algorithm.")
    }
    cpts <- args[["cpts"]]
    if (!is.list(cpts)) {
      cpts <- list(cpts)
    }
    mod <- new_seg_default(x, cpt_list = cpts)
  }
  if (method == "null") {
    mod <- new_seg_default(x)
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
  ll <- logLik(object$nhpp)
  attr(ll, "df") <- length(changepoints(object))
  class(ll) <- "logLik"
  return(ll)
}

#' @rdname changepoints
#' @export
MBIC <- function(object, ...) UseMethod("MBIC")

#' @rdname changepoints
#' @references Zhang and Seigmmund (2007) for MBIC: \doi{10.1111/j.1541-0420.2006.00662.x}
#' @export
MBIC.tidycpt <- function(object, ...) {
  MBIC(object$nhpp)
}

#' @rdname changepoints
#' @export

BMDL <- function(x, ...) UseMethod("BMDL")

#' @rdname changepoints
#' @export
#' @examples
#' x <- segment(DataCPSim, method = "cpt-pelt")
#' BMDL(x)
#' y <- segment(DataCPSim, method = "manual", cpts = 826)
#' BMDL(y)
#' z <- segment(DataCPSim, method = "single-best")
#' BMDL(z)

BMDL.tidycpt <- function(x, ...) {
  BMDL(x$nhpp)
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
      nhpp_logLik = logLik(x),
      nhpp_AIC = AIC(x),
      nhpp_BIC = BIC(x),
      nhpp_MBIC = MBIC(x),
      nhpp_BMDL = BMDL(x)
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
    ggplot2::geom_hline(yintercept = mean(as.ts(x)), linetype = 3) +
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
    ggplot2::scale_y_continuous("Original Measurement") + 
    ggplot2::labs(
      title = "Original times series",
      subtitle = paste("Mean value is", round(mean(as.ts(x)), 2))
    )
}

#' @rdname changepoints
#' @export
#' @examples
#' plot_mcdf(segment(DataCPSim))

plot_mcdf <- function(x, ...) {
  n <- length(x)
  
  z <- exceedances(x) |>
    tibble::enframe(name = "cum_exceedances", value = "t_exceedance") |>
    dplyr::mutate(
      m = mcdf(x$nhpp),
      lower = stats::qpois(0.05, lambda = m),
      upper = stats::qpois(0.95, lambda = m),
    ) |>
    # always add the last observation
    dplyr::bind_rows(
      data.frame(
        cum_exceedances = c(0, length(exceedances(x))), 
        t_exceedance = c(0, n)
      )
    ) |>
    dplyr::distinct()
  
  regions <- tidy(x)
  ggplot2::ggplot(data = z, ggplot2::aes(x = t_exceedance, y = cum_exceedances)) +
    ggplot2::geom_vline(data = regions, ggplot2::aes(xintercept = end), linetype = 3) +
    ggplot2::geom_abline(intercept = 0, slope = 0.5, linetype = 3) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Time Index (t)", limits = c(0, n)) +
    ggplot2::scale_y_continuous("Cumulative Number of Exceedances (N)") +
    ggplot2::geom_line(ggplot2::aes(y = m), color = "red") +
    ggplot2::geom_line(ggplot2::aes(y = lower), color = "blue") +
    ggplot2::geom_line(ggplot2::aes(y = upper), color = "blue") +
    ggplot2::labs(
      title = "Exceedances of the mean over time",
      subtitle = paste("Total exceedances:", length(exceedances(x)))
    )
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
  patchwork::wrap_plots(plot(x), plot_mcdf(x), ncol = 1)
}

#' @rdname changepoints
#' @param data_name_slug character string that will identify the data set used
#' in the file name
#' @export
#' @examples
#' file_name(segment(DataCPSim, method = "cpt-pelt"))

file_name <- function(x, data_name_slug = "data") {
  glance(x) |>
    dplyr::select(dplyr::matches("algorithm|params|nhpp_BMDL")) |>
    dplyr::mutate(
      label = paste(
        data_name_slug, 
        algorithm,
        floor(BMDL(x)),
        x$segmenter$params |> cli::hash_obj_md5(),
        sep = "_"
      ),
      filename = paste0(label, ".rda")
    ) |>
    dplyr::pull(filename)
}
