globalVariables(c(
  "index", "region", "algorithm", "label", "filename", "y", "begin", "end", 
  "sd", "num_generation", "method", "m", "t_exceedance", "cum_exceedances",
  "lower", "upper", "model"
))

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

#' @rdname tidycpt-generics
#' @export
length.tidycpt <- function(x, ...) {
  length(as.ts(x))
}

#' @rdname tidycpt-generics
#' @inheritParams stats::nobs
#' @seealso [stats::nobs()]
#' @export
nobs.tidycpt <- function(object, ...) {
  nobs(object$segmenter)
}

#' @rdname fitness
#' @export
fitness.tidycpt <- function(object, ...) {
  fitness(object$segmenter)
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
  others <- list(
    segment(as.ts(x), method = "pelt"),
    segment(as.ts(x), method = "binseg"),
    segment(as.ts(x), method = "wbs"),
    segment(as.ts(x), method = "random"),
    segment(as.ts(x), method = "null")
  ) |>
    purrr::map(glance)
  dplyr::bind_rows(glance(x), others) |>
    dplyr::mutate(model = gsub(pattern = "fit_", replacement = "", model))
}

#' @rdname tidycpt-generics
#' @param use_time_index Should the x-axis labels be the time indices? Or the 
#' time labels? 
#' @export
#' @examples
#' plot(segment(CET, method = "pelt"))
#' plot(segment(CET, method = "pelt"), use_time_index = TRUE)
plot.tidycpt <- function(x, use_time_index = FALSE, ...) {
  regions <- tidy(x)
  g <- ggplot2::ggplot(
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
    ggplot2::scale_y_continuous("Original Measurement") + 
    ggplot2::labs(
      title = "Original times series",
      subtitle = paste("Mean value is", round(mean(as.ts(x), na.rm = TRUE), 2))
    )
  if (use_time_index) {
    my_labels <- function(t) {
      n <- length(t)
      indices <- 1:length(x)
      good <- t %in% indices
      out <- x$time_index[ifelse(good, t, NA)] |>
        as.character()
      replace(out, is.na(out), "")
    }
    
    g +
      ggplot2::scale_x_continuous("Time", labels = my_labels)
  } else {
    g +
      ggplot2::scale_x_continuous("Time Index (t)")
  }
}

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
        floor(BMDL(x$nhpp)),
        params(x$segmenter) |> cli::hash_obj_md5(),
        sep = "_"
      ),
      filename = paste0(label, ".rda")
    ) |>
    dplyr::pull(filename)
}
