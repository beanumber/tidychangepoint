globalVariables(c("bmdl", "nhpp", "cpt_length", "value"))

#' Default class for candidate changepoint sets
#' @export
#' @param x a numeric vector coercible into a `ts` object
#' @param cpt_list a possibly empty `list()` of candidate changepoints
#' @param params a possibly empty `list()` of parameters
#' @param ... currently ignored
#' @examples
#' seg <- seg_default(DataCPSim)
#' str(seg)
#' as.ts(seg)
#' changepoints(seg)

new_seg_default <- function(x = numeric(), cpt_list = list(), params = list(), ...) {
  stopifnot(is.numeric(x))
  structure(
    list(
      data = stats::as.ts(x),
      candidates = evaluate_cpts(cpt_list, .data = stats::as.ts(x)),
      params = params
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

#' @rdname new_seg_default
#' @export
as.ts.seg_default <- function(x, ...) {
  as.ts(x$data)
}

#' @rdname new_seg_default
#' @export
exceedances.seg_default <- function(x, ...) {
  exceedances(as.ts(x))
}

#' @rdname new_seg_default
#' @param object A `seg_default` object
#' @export
nobs.seg_default <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname new_seg_default
#' @export
length.seg_default <- function(x, ...) {
  length(as.ts(x))
}

#' @rdname new_seg_default
#' @export
changepoints.seg_default <- function(x, ...) {
  x$candidates |>
    dplyr::arrange(bmdl) |>
    utils::head(1) |>
    dplyr::pull(changepoints) |>
    purrr::pluck(1) |>
    as.integer()
}

#' @rdname cpt_gbmdl
#' @export

BMDL.seg_default <- function(x, ...) {
  x$candidates |>
    dplyr::arrange(bmdl) |>
    utils::head(1) |>
    dplyr::pull(bmdl)
}

#' @rdname new_seg_default
#' @export
logLik.seg_default <- function(object, ...) {
  regions <- fit_nhpp(object, tau = changepoints(object))
  log_likes <- sum(regions$logLik)
  attr(log_likes, "df") <- length(changepoints(object))
  class(log_likes) <- "logLik"
  return(log_likes)
}

#' @rdname new_seg_default
#' @export
evaluate_cpts <- function(x, ...) UseMethod("evaluate_cpts")

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
      bmdl = purrr::map_dbl(nhpp, BMDL)
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

#' @rdname new_seg_default
#' @export
params.seg_default <- function(x, ...) {
  x$params
}


#' @rdname new_seg_default
#' @export
glance.seg_default <- function(x, ...) {
  tibble::tibble(
    pkg = "tidychangepoint",
    version = utils::packageVersion("tidychangepoint"),
    algorithm = NA,
    params = list(x$params),
    num_cpts = length(changepoints(x))
  )
}

#' @rdname new_seg_default
#' @export

plot.seg_default <- function(x, ...) {
  plot_history(x)
}

#' @rdname new_seg_default
#' @export
#' @examples
#' diagnose(lista_AG)
#' diagnose(lista_AG$segmenter)
diagnose.seg_default <- function(x, ...) {
  patchwork::wrap_plots(
    plot_best_chromosome(x),
    plot_cpt_repeated(x),
    ncol = 1
  )
}


#' @rdname changepoints
#' @export
#' @examples
#' plot_history(segment(DataCPSim, method = "random", k = 10)$segmenter)
#' plot_history(lista_AG$segmenter)
plot_history <- function(x, ...) {
  methods <- c("null", "single-best", "cpt-pelt")
  bmdls <- methods |>
    purrr::map(~segment(as.ts(x), method = .x)) |>
    purrr::map_dbl(BMDL)
  
  guidelines <- tibble::tibble(
    method = c(class(x)[1], methods),
    bmdl = c(BMDL(x), bmdls)
  )
  
  bmdl_seg <- x$candidates |>
    dplyr::mutate(
      num_generation = dplyr::row_number()
    )
  k <- num_generations(x)
  
  best <- bmdl_seg |> 
    dplyr::arrange(bmdl) |> 
    utils::head(1)
  
  ggplot2::ggplot(data = bmdl_seg, ggplot2::aes(x = num_generation, y = bmdl)) +
    ggplot2::geom_hline(
      data = guidelines, 
      ggplot2::aes(yintercept = bmdl, color = method), 
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


#' @rdname new_seg_default
#' @export
#' @examples
#' plot_best_chromosome(lista_AG$segmenter)
plot_best_chromosome <- function(x) {
  d <- x$candidates |> 
    dplyr::mutate(
      num_generation = dplyr::row_number(),
      cpt_length = purrr::map_int(changepoints, length)
    )
  best <- d |> 
    dplyr::arrange(bmdl) |> 
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

#' @rdname new_seg_default
#' @param i index of candidates to show
#' @export
#' @examples
#' plot_cpt_repeated(lista_AG$segmenter)
#' plot_cpt_repeated(lista_AG$segmenter, 5)
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
