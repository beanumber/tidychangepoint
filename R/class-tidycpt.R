globalVariables(c(
  "index", "region", "algorithm", "label", "filename", "y", "begin", "end", 
  "sd", "num_generation", "method", "m", "t_exceedance", "cum_exceedances",
  "lower", "upper", "model", "elapsed_time"
))

#' Container class for `tidycpt` objects
#' @name tidycpt-class
#' @details Every `tidycpt` object contains:
#' - `segmenter`: The object returned by the underlying changepoint
#' detection algorithm. These can be of arbitrary class. Use [as.segmenter()]
#' to retrieve them. 
#' - `model`: A model object inheriting from [mod_cpt], as created by
#' [as.model()] when called on the `segmenter`. 
#' - `elapsed_time`: The clock time that passed while the algorithm was running.
#' - `time_index`: If available, the labels for the time indices of the time series.
#' @returns A [tidycpt-class] object.
#' @examples
#' # Segment a time series using PELT
#' x <- segment(CET, method = "pelt")
#' class(x)
#' str(x)
#' 
NULL

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

#' @rdname reexports
#' @export
as.ts.tidycpt <- function(x, ...) {
  as.ts(x$segmenter)
}

#' @rdname as.model
#' @export
as.model.tidycpt <- function(object, ...) {
  object$model
}

#' @rdname as.segmenter
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

#' @rdname reexports
#' @export
augment.tidycpt <- function(x, ...) {
  augment(x$model)
}

#' @rdname reexports
#' @export
tidy.tidycpt <- function(x, ...) {
  tidy(x$model)
}

#' @rdname reexports
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

#' Compare various models or algorithms for a given changepoint set
#' @param x A [tidycpt-class] object
#' @param ... currently ignored
#' @details
#' A [tidycpt-class] object has a set of changepoints returned by the 
#' algorithm that segmented the time series. 
#' That changepoint set was obtained using a specific model. 
#' Treating this changepoint set as fixed, the [compare_models()] function
#' fits several common changepoint models to the time series and changepoint 
#' set, and returns the results of [glance()]. 
#' Comparing the fits of various models could lead to improved understanding.  
#' 
#' @returns A [tibble::tbl_df]
#' @export
#' @examples
#' 
#' # Segment a times series using PELT
#' x <- segment(CET, method = "pelt")
#' 
#' # Compare models
#' compare_models(x)
#' 
#' # Compare algorithms
#' compare_algorithms(x)
#' 
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
#' @details
#' Alternatively, [compare_algorithms()] runs several fast changepoint detection
#' algorithms on the original time series, and consolidates the results.
#' 
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

#' @rdname reexports
#' @param use_time_index Should the x-axis labels be the time indices? Or the 
#' time labels? 
#' @export
#' @examples
#' # Plot a segmented time series
#' plot(segment(CET, method = "pelt"))
#' 
#' # Plot a segmented time series and show the time labels on the x-axis
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

#' @rdname reexports
#' @export
print.tidycpt <- function(x, ...) {
  cat("A tidycpt object\n")
  print(x$segmenter)
  print(x$model)
}

#' @rdname regions
#' @export
regions.tidycpt <- function(x, ...) {
  regions(x$model)
}

#' @rdname diagnose
#' @export
#' @examples
#' # Show various iterations of diagnostic plots
#' diagnose(segment(DataCPSim))
#' diagnose(segment(DataCPSim, method = "single-best"))
#' diagnose(segment(DataCPSim, method = "pelt"))
#' 
#' # Show diagnostic plots for test sets
#' diagnose(segment(test_set()))
#' diagnose(segment(test_set(n = 2, sd = 4), method = "pelt"))
#' 
diagnose.tidycpt <- function(x, ...) {
  patchwork::wrap_plots(plot(x), diagnose(x$model), ncol = 1)
}

#' Obtain a descriptive filename for a tidycpt object
#' @param x A [tidycpt-class] object
#' @param data_name_slug character string that will identify the data set used
#' in the file name
#' @details
#' [file_name()] generates a random, unique string indicating the algorithm and 
#' [fitness()] for a [tidycpt-class] object.
#' 
#' @export
#' @returns A `character` string giving a unique file name.
#' @examples
#' # Generate a unique name for the file
#' DataCPSim |>
#'   segment(method = "pelt") |>
#'   file_name()

file_name <- function(x, data_name_slug = "data") {
  glance(x) |>
    dplyr::select(dplyr::matches("algorithm|params|MDL")) |>
    dplyr::mutate(
      label = paste(
        data_name_slug, 
        algorithm,
        floor(fitness(x)),
        seg_params(x$segmenter) |> cli::hash_obj_md5(),
        sep = "_"
      ),
      filename = paste0(label, ".rda")
    ) |>
    dplyr::pull(filename)
}
