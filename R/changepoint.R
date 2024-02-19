#' Compatibility layer for changepoint
#' @param x A `cpt` object returned by [changepoint::cpt.meanvar()]
#' @param ... arguments passed to methods
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "cpt-pelt")
#' class(cpts)
#' y <- augment(cpts)
#' class(y)
#' y
#' tidy(cpts)
#' glance(cpts)

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

#' @rdname glance.cpt
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "cpt-pelt")
#' as.ts(cpts)
#' 
as.ts.cpt <- function(x, ...) {
  as.ts(x@data.set)
}

#' @rdname glance.cpt
#' @param object A `cpt` object.
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "cpt-pelt")
#' nobs(cpts)
#' 
nobs.cpt <- function(object, ...) {
  length(as.ts(object@data.set))
}

#' @rdname glance.cpt
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "cpt-pelt", penalty = "BIC")
#' logLik(cpts)
#' cpts <- segment(DataCPSim, method = "cpt-pelt", penalty = "AIC")
#' logLik(cpts)
#' 
logLik.cpt <- function(object, ...) {
#  message("intercepting...")
  y <- changepoint::likelihood(object) |>
    suppressWarnings()
  ll <- -y[1] / 2
  attr(ll, "df") <- length(object@cpts)
  class(ll) <- "logLik"
  return(ll)
}

#' @rdname glance.cpt
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "cpt-pelt")
#' changepoints(cpts)
#' 
changepoints.cpt <- function(x, ...) {
  changepoint::cpts(x) |>
    as.integer()
}
