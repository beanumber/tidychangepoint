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
      candidates = tibble::tibble(changepoints = cpt_list),
      params = params
    ), 
    class = "seg_default"
  ) |>
  evaluate()
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
evaluate <- function(x, ...) {
  x$candidates <- x$candidates |>
    dplyr::mutate(
      nhpp = purrr::map(changepoints, ~fit_nhpp(x = as.ts(x), tau = .x)),
      bmdl = purrr::map_dbl(nhpp, BMDL)
    )
  ll <- x$candidates$nhpp |>
    purrr::map(logLik)
  x$candidates$logLik <- as.numeric(ll)
  x$candidates <- x$candidates |>
    dplyr::mutate(
      AIC = purrr::map_dbl(nhpp, AIC),
      BIC = purrr::map_dbl(nhpp, BIC),
      MBIC = purrr::map_dbl(nhpp, MBIC),
    ) |>
    dplyr::arrange(bmdl)
  x
}


#' @rdname new_seg_default
#' @export
glance.seg_default <- function(x, ...) {
  tibble::tibble(
    algorithm = NA,
    params = list(x$params)
  )
}

