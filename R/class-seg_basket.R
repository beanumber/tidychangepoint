globalVariables(c("bmdl", "nhpp", "cpt_length", "value", ".data"))

#' Default class for candidate changepoint sets
#' @export
#' @inheritParams new_seg_cpt
#' @param cpt_list a possibly empty `list()` of candidate changepoints
#' @examples
#' seg <- seg_basket(DataCPSim, cpt_list = list(c(365), c(330, 839)))
#' str(seg)
#' as.ts(seg)
#' changepoints(seg)
#' fitness(seg)
new_seg_basket <- function(x = numeric(), 
                            algorithm = NA, 
                            cpt_list = list(), 
                            seg_params = list(), 
                            model_name = "meanshift_norm", 
                            penalty = "BIC", ...) {
  stopifnot(is.numeric(x))
  structure(
    list(
      data = stats::as.ts(x),
      algorithm = algorithm,
      basket = cpt_list |>
        evaluate_cpts(.data = stats::as.ts(x), model_fn = whomademe(model_name)),
      seg_params = seg_params,
      model_name = model_name,
      penalty = penalty
    ), 
    class = "seg_basket"
  )
}

#' @rdname new_seg_basket
#' @export

validate_seg_basket <- function(x) {
  if (!stats::is.ts(as.ts(x))) {
    stop("data attribute is not coercible into a ts object.")
  }
  x
}

#' @rdname new_seg_basket
#' @export

seg_basket <- function(x, ...) {
  obj <- new_seg_basket(x, ...)
  validate_seg_basket(obj)
}

#' Methods for seg_basket objects
#' @name seg-basket-generics
#' @param object A `seg_basket` object
#' @param ... arguments passed to methods
#' @export
as.segmenter.seg_basket <- function(object, ...) {
  seg_cpt(
    x = as.ts(object$data),
    pkg = "tidychangepoint",
    algorithm = object$algorithm,
    changepoints = changepoints(object),
    seg_params = list(object$seg_params),
    model = object$model_name,
    fitness = fitness(object)
  )
}

#' @rdname seg-basket-generics
#' @param x A `seg_basket` object
#' @export
as.ts.seg_basket <- function(x, ...) {
  x$data
}

#' @rdname seg-basket-generics
#' @export
best_cpt <- function(x, ...) {
  x$basket |>
    dplyr::arrange(.data[[x$penalty]]) |>
    utils::head(1)
}

#' @rdname changepoints
#' @export
changepoints.seg_basket <- function(x, ...) {
  best_cpt(x, ...)|>
    dplyr::pull(changepoints) |>
    purrr::pluck(1) |>
    as.integer()
}

#' @rdname fitness
#' @export
fitness.seg_basket <- function(object, ...) {
  if (nrow(object$basket) == 0) {
    out <- NA
  } else {
    out <- best_cpt(object, ...) |>
      dplyr::pull(.data[[object$penalty]])
  }
  names(out) <- object$penalty
  out
}

#' @rdname new_seg_basket
#' @param model_fn Name of the function to fit the model. 
#' See, for examples, [fit_meanshift()].
#' @export
evaluate_cpts.seg_basket <- function(x, ...) {
  evaluate_cpts(x$basket, .data = as.ts(x), model_fn = whomademe(x), ...)
}

#' @rdname new_seg_basket
#' @param .data A time series
#' @export
evaluate_cpts.list <- function(x, .data, model_fn, ...) {
  tibble::tibble(changepoints = x) |>
    evaluate_cpts(.data = .data, model_fn = model_fn, ...)
}

#' @rdname new_seg_basket
#' @export
evaluate_cpts.tbl_df <- function(x, .data, model_fn, ...) {
  if (!stats::is.ts(as.ts(.data))) {
    stop(".data must be coercible into a time series")
  }
  if (!inherits(model_fn, "fun_cpt")) {
    stop("model_fn must be a fun_cpt function")
  }
  y <- x
  y$model <- x$changepoints |>
    purrr::map(model_fn, x = .data)
#  WHY???????
#  y <- x |>
#    dplyr::mutate(
#      model = purrr::map(changepoints, ~model_fn(x = .data, tau = .x))
#    )
  ll <- y$model |>
    purrr::map(logLik)
  y$logLik <- as.numeric(ll)
  y$AIC = purrr::map_dbl(y$model, AIC)
  y$BIC = purrr::map_dbl(y$model, BIC)
  y$MBIC = purrr::map_dbl(y$model, MBIC)
  y$MDL = purrr::map_dbl(y$model, MDL)
  # WHY ?????
  # out <- y |>
  #   dplyr::mutate(
  #     AIC = purrr::map_dbl(model, AIC),
  #     BIC = purrr::map_dbl(model, BIC),
  #     MBIC = purrr::map_dbl(model, MBIC),
  #     MDL = purrr::map_dbl(model, MDL)
  #   )  
  if (model_name(model_fn) == "nhpp") {
    y$BMDL = purrr::map_dbl(y$model, BMDL)
  #  y <- y |>
  #      dplyr::mutate(BMDL = purrr::map_dbl(model, BMDL))
  } else {
    y
  }
  return(y)
}

#' @rdname model_name
#' @export
model_name.seg_basket <- function(object, ...) {
  object$model_name
}

#' @rdname seg-basket-generics
#' @export
plot.seg_basket <- function(x, ...) {
  methods <- c("null", "pelt")
  penalty <- names(fitness(x))
  f <- whomademe(x)
  vals <- methods |>
    purrr::map(~segment(as.ts(x), method = .x)) |>
    purrr::map(changepoints) |>
    purrr::map(~f(as.ts(x), tau = .x)) |>
    purrr::map_dbl(eval(parse(text = penalty)))
  
  guidelines <- tibble::tibble(
    method = c(class(x)[1], methods),
    value = c(fitness(x), vals)
  )
  
  seg <- x$basket |>
    dplyr::mutate(
      num_generation = dplyr::row_number()
    )
  
  best <- seg |>
    dplyr::arrange(.data[[x$penalty]]) |>
    utils::head(1)
  
  ggplot2::ggplot(data = seg, ggplot2::aes(x = num_generation, y = .data[[x$penalty]])) +
    ggplot2::geom_hline(
      data = guidelines, 
      ggplot2::aes(yintercept = value, color = method), 
      linetype = 2
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = best$num_generation, linetype = 3) + 
    ggplot2::geom_point(data = best) +
    ggplot2::geom_smooth(se = 0) + 
    ggplot2::scale_x_continuous("Generation of Candidate Changepoints") +
    ggplot2::labs(
      title = "Evolution of Objective Function values",
      subtitle = "Comparison with other known algorithms"
    )
}

#' @rdname diagnose
#' @export
#' @examples
#' x <- segment(DataCPSim, method = "coen", num_generations = 3)
#' plot(x$segmenter)
#' diagnose(x$segmenter)
diagnose.seg_basket <- function(x, ...) {
  patchwork::wrap_plots(
    plot_best_chromosome(x),
    plot_cpt_repeated(x),
    ncol = 1
  )
}


#' @rdname seg-basket-generics
#' @export
#' @examples
#' x <- segment(DataCPSim, method = "coen", num_generations = 3)
#' plot_best_chromosome(x$segmenter)
plot_best_chromosome <- function(x) {
  d <- x$basket |> 
    dplyr::mutate(
      num_generation = dplyr::row_number(),
      cpt_length = purrr::map_int(changepoints, length)
    )
  best <- d |> 
    dplyr::arrange(.data[[x$penalty]]) |> 
    utils::head(1)
  ggplot2::ggplot(d, ggplot2::aes(x = num_generation, y = cpt_length)) +
    ggplot2::geom_hline(
      yintercept = best |> dplyr::pull(cpt_length),
      linetype = 3,
      color = "blue"
    ) +
    ggplot2::geom_vline(xintercept = best$num_generation, linetype = 3) + 
    ggplot2::geom_line() +
    ggplot2::geom_point(data = best) +
    ggplot2::scale_x_continuous("Generation of Candidate Changepoints") +
    ggplot2::scale_y_continuous("Number of changepoints in set") +
    ggplot2::labs(
      title = "Evolution of changepoint set size",
      subtitle = paste("The best changepoint set has", best |> dplyr::pull(cpt_length), "change points")
    )
}

#' @rdname seg-basket-generics
#' @param i index of basket to show
#' @export
#' @examples
#' x <- segment(DataCPSim, method = "coen", num_generations = 3)
#' plot_cpt_repeated(x$segmenter)
#' plot_cpt_repeated(x$segmenter, 5)
plot_cpt_repeated <- function(x, i = nrow(x$basket)) {
  
  x$basket |>
    dplyr::slice(1:i) |>
    dplyr::select(changepoints) |>
    tidyr::unnest(changepoints) |>
    ggplot2::ggplot(ggplot2::aes(x = changepoints)) +
    ggplot2::geom_vline(
      data = changepoints(x) |>
        tibble::enframe(), 
      ggplot2::aes(xintercept = value), 
      linetype = 3
    ) +
    ggplot2::geom_histogram() +
    ggplot2::scale_x_continuous(
      "Time Index (t)",
      limits = c(0, length(as.ts(x)))
    ) +
    ggplot2::scale_y_continuous(
      paste("Frequency of Appearance in", i, "Generations")
    ) +
    ggplot2::labs(
      title = "Histogram of changepoint selections"
    )
}
