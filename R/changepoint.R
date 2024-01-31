#' Compatibility layer for changepoint
#' @export
#' @exportS3Method broom::augment
#' @examples
#' cpts <- segment(DataCPSim, method = "cpt-pelt")
#' class(cpts)
#' y <- augment(cpts)
#' class(y)
#' y
#' tidy(cpts)
#' glance(cpts)

augment.cpt <- function(x, ...) {
  cpt <- changepoint::cpts(x)
  data <- x@data.set 
  n <- length(data)
  data |>
    tsibble::as_tsibble() |>
    dplyr::mutate(region = cut(
      index, 
      breaks = c(0, cpt, n), 
      include.lowest = TRUE, 
      right = FALSE)
    ) |>
  dplyr::group_by(region)
}

#' @rdname augment.cpt
#' @export
#' @exportS3Method broom::tidy

tidy.cpt <- function(x, ...) {
  augment(x) |>
    dplyr::ungroup() |>
    # why is this necessary????
    as.data.frame() |>
    dplyr::group_by(region) |>
    dplyr::summarize(
      num_obs = dplyr::n(),
      min = min(value),
      max = max(value),
      mean = mean(value),
      sd = sd(value),
      ... = ...
    )
}

#' @rdname augment.cpt
#' @export
#' @exportS3Method broom::glance

glance.cpt <- function(x, ...) {
  out <- tibble::tibble(
    pkg = "changepoint",
    version = x@version,
    algorithm = x@method,
    test_stat = x@test.stat,
    # x@pen.type: need tidy eval in here somewhere????
    penalty = x@pen.value,
    num_cpts = length(x@cpts),
    num_cpts_max = x@ncpts.max,
    min_seg_length = x@minseglen
  )
  # hack
  names(out)[5] <- x@pen.type
  out
}

#' @rdname augment.cpt
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "cpt-pelt")
#' as.ts(cpts)
#' 
as.ts.cpt <- function(x, ...) {
  as.ts(x@data.set)
}

#' @rdname augment.cpt
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "cpt-pelt")
#' changepoints(cpts)
#' 
changepoints.cpt <- function(x, ...) {
  changepoint::cpts(x)
}
