globalVariables(c("bmdl", "nhpp", "cpt_length", "value", ".data"))

#' Default class for candidate changepoint sets
#' @export
#' @param x a numeric vector coercible into a `ts` object
#' @param algorithm Algorithm used to find the changepoints
#' @param cpt_list a possibly empty `list()` of candidate changepoints
#' @param params a possibly empty `list()` of parameters
#' @param model_name character indicating the model used to find the changepoints. 
#' @param penalty character indicating the name of the penalty function used to
#' find the changepoints.
#' @param ... currently ignored
#' @examples
#' seg <- seg_default(DataCPSim, cpt_list = list(c(365), c(330, 839)))
#' str(seg)
#' as.ts(seg)
#' changepoints(seg)
#' fitness(seg)
#' glance(seg)

new_seg_default <- function(x = numeric(), 
                            algorithm = NA, 
                            cpt_list = list(), 
                            params = list(), 
                            model_name = "meanshift", 
                            penalty = "BIC", ...) {
  stopifnot(is.numeric(x))
  structure(
    list(
      data = stats::as.ts(x),
      algorithm = algorithm,
      candidates = cpt_list |>
        evaluate_cpts(.data = stats::as.ts(x), model_fn = model_fit(model_name)),
      params = params,
      model_name = model_name,
      penalty = penalty
    ), 
    class = "seg_default"
  )
}

#' @rdname new_seg_default
#' @export

validate_seg_default <- function(x) {
  if (!stats::is.ts(as.ts(x))) {
    stop("data attribute is not coercible into a ts object.")
  }
  x
}

#' @rdname new_seg_default
#' @export

seg_default <- function(x, ...) {
  obj <- new_seg_default(x, ...)
  validate_seg_default(obj)
}

#' Methods for seg_default objects
#' @name seg-default-generics
#' @param x An `seg_default` object
#' @param ... arguments passed to methods
#' @export
as.ts.seg_default <- function(x, ...) {
  as.ts(x$data)
}

#' @rdname seg-default-generics
#' @param object A `seg_default` object
#' @export
nobs.seg_default <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname new_seg_default
#' @export
num_candidates <- function(x) {
  nrow(x$candidates)
}

#' @rdname cpt-generics
#' @export
best_cpt <- function(x, ...) {
  x$candidates |>
    dplyr::arrange(.data[[x$penalty]]) |>
    utils::head(1)
}

#' @rdname changepoints
#' @export
changepoints.seg_default <- function(x, ...) {
  best_cpt(x, ...)|>
    dplyr::pull(changepoints) |>
    purrr::pluck(1) |>
    as.integer()
}

#' @rdname fitness
#' @export
fitness.seg_default <- function(object, ...) {
  if (nrow(object$candidates) == 0) {
    out <- NA
  } else {
    out <- best_cpt(object, ...) |>
      dplyr::pull(.data[[object$penalty]])
  }
  names(out) <- object$penalty
  out
}

#' @rdname model_name
#' @export
model_name.seg_default <- function(object, ...) {
  object$model_name
}

#' @rdname model_args
#' @export
model_args.seg_default <- function(object, ...) {
  NA
}

#' @rdname new_seg_default
#' @param model_fn Name of the function to fit the model. 
#' See, for examples, [fit_meanshift()].
#' @export
evaluate_cpts.seg_default <- function(x, model_fn, ...) {
  evaluate_cpts(x$candidates, .data = as.ts(x), model_fn = model_fit(x), ...)
}

#' @rdname new_seg_default
#' @export
evaluate_cpts.list <- function(x, model_fn, ...) {
  tibble::tibble(changepoints = x) |>
    evaluate_cpts(model_fn = model_fn, ...)
}

#' @rdname new_seg_default
#' @export
evaluate_cpts.tbl_df <- function(x, model_fn, ...) {
  args <- list(...)
  if (!".data" %in% names(args)) {
    stop("This method requires a .data argument")
  } else {
    ds <- args[[".data"]]
  }
  if (!stats::is.ts(as.ts(ds))) {
    stop(".data must be coercible into a time series")
  }
  y <- x |>
    dplyr::mutate(
      model = purrr::map(changepoints, ~model_fn(x = as.ts(ds), tau = .x)),
#      BMDL = purrr::map_dbl(nhpp, BMDL)
    )
  ll <- y$model |>
    purrr::map(logLik)
  y$logLik <- as.numeric(ll)
  out <- y |>
    dplyr::mutate(
      AIC = purrr::map_dbl(model, AIC),
      BIC = purrr::map_dbl(model, BIC),
      MBIC = purrr::map_dbl(model, MBIC),
      MDL = purrr::map_dbl(model, MDL)
    )  
  if (model_name(model_fn) == "nhpp") {
    out <- out |>
      dplyr::mutate(BMDL = purrr::map_dbl(model, BMDL))
  } else {
    out
  }
}

#' @rdname seg-default-generics
#' @export
params.seg_default <- function(x, ...) {
  x$params
}

#' @rdname seg-default-generics
#' @export
glance.seg_default <- function(x, ...) {
  tibble::tibble(
    pkg = "tidychangepoint",
    version = utils::packageVersion("tidychangepoint"),
    algorithm = x$algorithm,
    params = list(x$params),
    num_cpts = length(changepoints(x)),
    model = model_name(x),
    criteria = names(fitness(x)),
    fitness = fitness(x)
  )
}

#' @rdname seg-default-generics
#' @export

plot.seg_default <- function(x, ...) {
  plot_history(x)
}

#' @rdname diagnose
#' @export
#' @examples
#' x <- segment(DataCPSim, method = "random", num_generations = 5)
#' diagnose(x)
#' diagnose(x$segmenter)
diagnose.seg_default <- function(x, ...) {
  patchwork::wrap_plots(
    plot_best_chromosome(x),
    plot_cpt_repeated(x),
    ncol = 1
  )
}

#' Plot seg_default information
#' @param x A `seg_default` object
#' @param ... currently ignored
#' @export
#' @examples
#' x <- segment(DataCPSim, method = "random", popSize = 10)
#' plot_history(x$segmenter)
plot_history <- function(x, ...) {
  methods <- c("null", "single-best", "pelt")
  vals <- methods |>
    purrr::map(~segment(as.ts(x), method = .x)) |>
    purrr::map(~purrr::pluck(.x, "model")) |>
    purrr::map_dbl(eval(parse(text = x$penalty)))
  
  guidelines <- tibble::tibble(
    method = c(class(x)[1], methods),
    value = c(fitness(x), vals)
  )
  
  seg <- x$candidates |>
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


#' @rdname plot_history
#' @export
#' @examples
#' x <- segment(DataCPSim, method = "random", num_generations = 10)
#' plot_best_chromosome(x$segmenter)
plot_best_chromosome <- function(x) {
  d <- x$candidates |> 
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

#' @rdname plot_history
#' @param i index of candidates to show
#' @export
#' @examples
#' x <- segment(DataCPSim, method = "random", k = 10)
#' plot_cpt_repeated(x$segmenter)
#' plot_cpt_repeated(x$segmenter, 5)
plot_cpt_repeated <- function(x, i = nrow(x$candidates)) {
  
  x$candidates |>
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
