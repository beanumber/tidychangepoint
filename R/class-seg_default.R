globalVariables(c("bmdl", "nhpp", "cpt_length", "value"))

#' Default class for candidate changepoint sets
#' @export
#' @param x a numeric vector coercible into a `ts` object
#' @param algorithm Algorithm used to find the changepoints
#' @param cpt_list a possibly empty `list()` of candidate changepoints
#' @param params a possibly empty `list()` of parameters
#' @inheritParams new_mod_default
#' @param ... currently ignored
#' @examples
#' seg <- seg_default(DataCPSim)
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
      candidates = evaluate_cpts(cpt_list, .data = stats::as.ts(x)),
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
best_nhpp <- function(x, ...) {
  x$candidates |>
    dplyr::arrange(.data[[x$penalty]]) |>
    utils::head(1) |>
    dplyr::pull(nhpp) |>
    purrr::pluck(1)
}


#' @rdname changepoints
#' @export
changepoints.seg_default <- function(x, ...) {
  x$candidates |>
    dplyr::arrange(.data[[x$penalty]]) |>
    utils::head(1) |>
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
    out <- object$candidates |>
      dplyr::arrange(.data[[object$penalty]]) |>
      utils::head(1) |>
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

#' @rdname new_seg_default
#' @export
evaluate_cpts.seg_default <- function(x, ...) {
  evaluate_cpts(x$candidates, .data = as.ts(x), ...)
}

#' @rdname new_seg_default
#' @export
evaluate_cpts.list <- function(x, ...) {
  tibble::tibble(changepoints = x) |>
    evaluate_cpts(...)
}

#' @rdname new_seg_default
#' @export
evaluate_cpts.tbl_df <- function(x, ...) {
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
      nhpp = purrr::map(changepoints, ~fit_nhpp(x = as.ts(ds), tau = .x)),
      BMDL = purrr::map_dbl(nhpp, BMDL)
    )
  ll <- y$nhpp |>
    purrr::map(logLik)
  y$logLik <- as.numeric(ll)
  y |>
    dplyr::mutate(
      AIC = purrr::map_dbl(nhpp, AIC),
      BIC = purrr::map_dbl(nhpp, BIC),
      MBIC = purrr::map_dbl(nhpp, MBIC),
    )
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
#' x <- segment(DataCPSim, method = "random", num_generations = 10)
#' plot_history(x$segmenter)
plot_history <- function(x, ...) {
  methods <- c("null", "single-best", "pelt")
  bmdls <- methods |>
    purrr::map(~segment(as.ts(x), method = .x)) |>
    purrr::map(~purrr::pluck(.x, "model")) |>
    purrr::map_dbl(MDL)
  
  guidelines <- tibble::tibble(
    method = c(class(x)[1], methods),
    BMDL = c(BMDL(best_nhpp(x)), bmdls)
  )
  
  bmdl_seg <- x$candidates |>
    dplyr::mutate(
      num_generation = dplyr::row_number()
    )

  best <- bmdl_seg |> 
    dplyr::arrange(BMDL) |> 
    utils::head(1)
  
  ggplot2::ggplot(data = bmdl_seg, ggplot2::aes(x = num_generation, y = BMDL)) +
    ggplot2::geom_hline(
      data = guidelines, 
      ggplot2::aes(yintercept = BMDL, color = method), 
      linetype = 2
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = best$num_generation, linetype = 3) + 
    ggplot2::geom_point(data = best) +
    ggplot2::geom_smooth(se = 0) + 
    ggplot2::scale_x_continuous("Generation of Candidate Changepoints") +
    ggplot2::scale_y_continuous("BMDL") +
    ggplot2::labs(
      title = "Evolution of BMDL scores",
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
    dplyr::arrange(BMDL) |> 
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
