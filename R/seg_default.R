#' Default class for candidate changepoint sets
#' @export
#' @param x a numeric vector coercible into a `ts` object
#' @param cpt_list a possibly empty `list()` of candidate changepoints
#' @param params a possibly empty `list()` of parameters
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
  if (!is.ts(as.ts(ds))) {
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
glance.seg_default <- function(x, ...) {
  tibble::tibble(
    algorithm = NA,
    params = list(x$params)
  )
}

