globalVariables(c(
  "index", "region", "algorithm", "label", "filename", "y", "begin", "end", 
  "sd", "num_generation", "method", "m", "t_exceedance", "cum_exceedances",
  "lower", "upper", "model", "elapsed_time"
))

#' @rdname changepoints
#' @param use_labels return the time labels for the changepoints instead of the
#' indices. 
#' @export
changepoints.tidycpt <- function(x, use_labels = FALSE, ...) {
  if (use_labels && length(x$time_index) == length(as.ts(x))) {
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

#' @rdname as.model
#' @details
#'   - [as.model.tidycpt()] returns the `model` object of a `tidycpt` object
#' 
#' @export
as.model.tidycpt <- function(object, ...) {
  object$model
}

#' @rdname as.model
#' @details
#'   - [as.segmenter.tidycpt()] returns the `segmenter` object of a `tidycpt` object
#' 
#' @export
as.segmenter.tidycpt <- function(object, ...) {
  object$segmenter
}


#' @rdname fitness
#' @export
fitness.tidycpt <- function(object, ...) {
  fitness(object$segmenter)
}

#' @rdname model_name
#' @export
model_name.tidycpt <- function(object, ...) {
  model_name(object$segmenter, ...)
}

#' @rdname tidycpt-generics
#' @seealso [broom::augment()]
#' @export
augment.tidycpt <- function(x, ...) {
  augment(x$model)
}

#' @rdname tidycpt-generics
#' @seealso [broom::tidy()]
#' @export
tidy.tidycpt <- function(x, ...) {
  tidy(x$model)
}

#' @rdname tidycpt-generics
#' @seealso [broom::glance()]
#' @export
glance.tidycpt <- function(x, ...) {
  x |>
    as.segmenter() |>
    as.seg_cpt() |>
    glance() |>
    dplyr::mutate(
      elapsed_time = round(x$elapsed_time, 3)
    )
}

#' Compare various models for a give changepoint set
#' @inheritParams diagnose
#' @param ... currently ignored
#' @export
compare_models <- function(x, ...) {
  list(
    x$model,
    fit_meanshift_norm(as.ts(x), tau = changepoints(x)),
    fit_meanshift_norm_ar1(as.ts(x), tau = changepoints(x)),
    fit_trendshift(as.ts(x), tau = changepoints(x)),
    fit_trendshift_ar1(as.ts(x), tau = changepoints(x)),
    fit_meanvar(as.ts(x), tau = changepoints(x)),
    fit_lmshift(as.ts(x), tau = changepoints(x), deg_poly = 2),
    fit_nhpp(as.ts(x), tau = changepoints(x))
  ) |>
    purrr::map(glance) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data[[names(fitness(x))]])
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
    unique() |>
    purrr::map(glance)
  dplyr::bind_rows(glance(x), others) |>
    dplyr::arrange(elapsed_time)
}

#' @rdname tidycpt-generics
#' @param use_time_index Should the x-axis labels be the time indices? Or the 
#' time labels? 
#' @export
#' @examples
#' plot(segment(CET, method = "pelt"))
#' plot(segment(CET, method = "pelt"), use_time_index = TRUE)
plot.tidycpt <- function(x, use_time_index = FALSE, ...) {
  g <- plot(x$model)
  b <- g |>
    ggplot2::ggplot_build() |>
    purrr::pluck("layout") |>
    purrr::pluck("panel_scales_x") |>
    purrr::pluck(1)
  if (use_time_index) {
    my_labels <- function(t) {
      n <- length(t)
      indices <- 1:nobs(x$model)
      good <- t %in% indices
      out <- x$time_index[ifelse(good, t, NA)] |>
        as.character()
      replace(out, is.na(out), "")
    }
    
    g <- g +
      ggplot2::scale_x_continuous("Time", breaks = b$breaks, labels = my_labels)
  }
  g
}

#' @rdname tidycpt-generics
#' @export
print.tidycpt <- function(x, ...) {
  cat("A tidycpt object\n")
  print(x$segmenter)
  print(x$model)
}

#' @rdname diagnose
#' @export
#' @examples
#' diagnose(segment(DataCPSim))
#' diagnose(segment(DataCPSim, method = "single-best"))
#' diagnose(segment(DataCPSim, method = "pelt"))
#' diagnose(segment(test_set()))
#' diagnose(segment(test_set(n = 2, sd = 4), method = "pelt"))
diagnose.tidycpt <- function(x, ...) {
  patchwork::wrap_plots(plot(x), diagnose(x$model), ncol = 1)
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
    dplyr::select(dplyr::matches("algorithm|params|MDL")) |>
    dplyr::mutate(
      label = paste(
        data_name_slug, 
        algorithm,
        floor(MDL(x$model)),
        seg_params(x$segmenter) |> cli::hash_obj_md5(),
        sep = "_"
      ),
      filename = paste0(label, ".rda")
    ) |>
    dplyr::pull(filename)
}
