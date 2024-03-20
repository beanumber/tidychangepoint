globalVariables(c(
  "index", "region", "algorithm", "label", "filename", "y", "begin", "end", 
  "sd", "num_generation", "method", "m", "t_exceedance", "cum_exceedances",
  "lower", "upper"
))

#' Extract changepoints
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
changepoints_labels <- function(x, ...) {
  if (length(x$time_index) == length(x)) {
    x$time_index[changepoints(x)]
  } else {
    changepoints(x$segmenter)
  }
}

#' Generic functions for tidycpt objects
#' @name tidycpt-generics
#' @param x An `tidycpt` object, typically the output from [segment()]`
#' @param ... arguments passed to methods
#' @export
as.ts.tidycpt <- function(x, ...) {
  as.ts(x$segmenter)
}

#' Compute exceedances of a threshold for a time series
#' 
#' @inheritParams segment
#' @export
exceedances <- function(x, ...) UseMethod("exceedances")

#' @rdname exceedances
#' @export
exceedances.tidycpt <- function(x, ...) {
  exceedances(as.ts(x), ...)
}

#' @rdname tidycpt-generics
#' @export
length.tidycpt <- function(x, ...) {
  length(as.ts(x))
}

#' @rdname tidycpt-generics
#' @param object A `tidycpt` object
#' @seealso [stats::logLik()]
#' @export
logLik.tidycpt <- function(object, ...) {
  logLik(object$nhpp)
}

#' Modified Bayesian Information Criterion
#' 
#' @inheritParams stats::logLik
#' @export
#' @seealso [stats::BIC()]
#' @references Zhang and Seigmmund (2007) for MBIC: \doi{10.1111/j.1541-0420.2006.00662.x}
MBIC <- function(object, ...) UseMethod("MBIC")

#' @rdname MBIC
#' @export
MBIC.tidycpt <- function(object, ...) {
  MBIC(object$nhpp)
}

#' Maximum Descriptive Length
#' 
#' @export
MDL <- function(object, ...) UseMethod("MDL")

#' Bayesian Maximum Descriptive Length
#' 
#' @inheritParams stats::logLik
#' @seealso [MDL()]
#' @export
BMDL <- function(object, ...) UseMethod("BMDL")

#' @rdname BMDL
#' @export
#' @examples
#' x <- segment(DataCPSim, method = "pelt")
#' BMDL(x)
#' y <- segment(DataCPSim, method = "manual", cpts = 826)
#' BMDL(y)
#' z <- segment(DataCPSim, method = "single-best")
#' BMDL(z)

BMDL.tidycpt <- function(object, ...) {
  BMDL(object$nhpp)
}


#' @rdname tidycpt-generics
#' @seealso [stats::nobs()]
#' @export
nobs.tidycpt <- function(object, ...) {
  nobs(object$segmenter)
}

#' @rdname tidycpt-generics
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

#' @rdname tidycpt-generics
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
      min = min(y, na.rm = TRUE),
      max = max(y, na.rm = TRUE),
      mean = mean(y, na.rm = TRUE),
      sd = stats::sd(y, na.rm = TRUE),
      ... = ...
    ) |>
    dplyr::inner_join(theta, by = "region")
}

#' @rdname tidycpt-generics
#' @seealso [broom::glance()]
#' @export
glance.tidycpt <- function(x, ...) {
  glance(x$segmenter) |>
    dplyr::mutate(
      elapsed_time = x$elapsed_time
    )
}

#' Compare various models for a give changepoint set
#' @inheritParams diagnose
#' @param ... currently ignored
#' @export
compare_models <- function(x, ...) {
  list(
    x$nhpp,
    fit_lmshift(as.ts(x), tau = changepoints(x), trends = FALSE),
    fit_lmshift(as.ts(x), tau = changepoints(x), trends = FALSE, ar1 = TRUE),
    fit_lmshift(as.ts(x), tau = changepoints(x), trends = TRUE),
    fit_lmshift(as.ts(x), tau = changepoints(x), trends = TRUE, ar1 = TRUE)
  ) |>
    purrr::map(glance) |>
    dplyr::bind_rows()
}

#' @rdname compare_models
#' @export
compare_algorithms <- function(x, ...) {
  list(
    segment(as.ts(x), method = "pelt"),
    segment(as.ts(x), method = "binseg"),
    segment(as.ts(x), method = "wbs"),
    segment(as.ts(x), method = "random"),
    segment(as.ts(x), method = "null")
  ) |>
    purrr::map(glance) |>
    dplyr::bind_rows()
}


#' @rdname changepoints
#' @export
params <- function(x, ...) UseMethod("params")

#' @rdname tidycpt-generics
#' @export
plot.tidycpt <- function(x, ...) {
  regions <- tidy(x)
  ggplot2::ggplot(
    data = augment(x), 
    ggplot2::aes(x = index, y = y)
  ) +
#    ggplot2::geom_rect(
#      data = regions,
#      ggplot2::aes(xmin = begin, xmax = end, ymin = 0, ymax = Inf, x = NULL, y = NULL),
#      fill = "grey90"
#    ) +
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
      subtitle = paste("Mean value is", round(mean(as.ts(x), na.rm = TRUE), 2))
    )
}

#' Diagnose the fit of a segmented time series
#' @param x A `tidycpt` object
#' @param ... currently ignored
#' @export
diagnose <- function(x, ...) UseMethod("diagnose")

#' @rdname diagnose
#' @export
#' @examples
#' diagnose(segment(DataCPSim))
#' diagnose(segment(DataCPSim, method = "single-best"))
#' diagnose(segment(DataCPSim, method = "pelt"))
#' diagnose(segment(test_set()))
#' diagnose(segment(test_set(n = 2, sd = 4), method = "pelt"))
#' 
diagnose.tidycpt <- function(x, ...) {
  patchwork::wrap_plots(plot(x), plot(x$nhpp), ncol = 1)
}

#' Obtain a descriptive filename for a tidycpt object
#' @inheritParams diagnose
#' @param data_name_slug character string that will identify the data set used
#' in the file name
#' @export
#' @examples
#' file_name(segment(DataCPSim, method = "pelt"))

file_name <- function(x, data_name_slug = "data") {
  glance(x) |>
    dplyr::select(dplyr::matches("algorithm|params|nhpp_BMDL")) |>
    dplyr::mutate(
      label = paste(
        data_name_slug, 
        algorithm,
        floor(BMDL(x)),
        params(x$segmenter) |> cli::hash_obj_md5(),
        sep = "_"
      ),
      filename = paste0(label, ".rda")
    ) |>
    dplyr::pull(filename)
}
