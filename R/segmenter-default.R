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
      cpt_list = cpt_list,
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
changepoints.seg_default <- function(x, ...) {
  x$cpt_list |>
    tibble::enframe(name = "id", value = "changepoints") |>
    dplyr::mutate(
      nhpp = purrr::map(changepoints, ~fit_nhpp(x = as.ts(x), tau = .x)),
      bmdl = purrr::map_dbl(nhpp, BMDL)
    ) |>
    dplyr::arrange(bmdl) |>
    utils::head(1) |>
    dplyr::pull(changepoints) |>
    purrr::pluck(1) |>
    as.integer()
}

#' @rdname new_seg_default
#' @export
glance.seg_default <- function(x, ...) {
  tibble::tibble(
    algorithm = NA,
    params = list(x$params)
  )
}


#' @rdname new_seg_default
#' @export
logLik.seg_default <- function(x, ...) {
  # need to make this work
  out <- -3248
  # Bloq_LogVero_NHPP(x, ...)
  attr(out, "df") <- length(cpt_best(x))
  class(out) <- "logLik"
  return(out)
}

