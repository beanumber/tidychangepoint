#' Compatibility layer for changepoint
#' @param x A `cpt` object returned by [changepoint::cpt.meanvar()]
#' @param ... arguments passed to methods
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "pelt")
#' class(cpts)
#' y <- augment(cpts)
#' class(y)
#' y
#' tidy(cpts)
#' glance(cpts)

glance.cpt <- function(x, ...) {
  tibble::tibble(
    pkg = "changepoint",
    version = package_version(x@version),
    algorithm = x@method,
    params = list(params(x)),
    num_cpts = length(changepoints(x))
  )
}

#' @rdname glance.cpt
#' @export
params.cpt <- function(x, ...) {
  out <- list(
    test_stat = x@test.stat,
    penalty = x@pen.value,
    num_cpts_max = x@ncpts.max,
    min_seg_length = x@minseglen
  )
  # hack
  names(out)[2] <- x@pen.type
  out
}

#' @rdname glance.cpt
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "pelt")
#' as.ts(cpts)
#' 
as.ts.cpt <- function(x, ...) {
  as.ts(x@data.set)
}

#' @rdname glance.cpt
#' @param object A `cpt` object.
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "pelt")
#' nobs(cpts)
#' 
nobs.cpt <- function(object, ...) {
  length(as.ts(object@data.set))
}

#' @rdname glance.cpt
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "pelt", penalty = "BIC")
#' logLik(cpts)
#' cpts <- segment(DataCPSim, method = "pelt", penalty = "AIC")
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
#' cpts <- segment(DataCPSim, method = "pelt")
#' changepoints(cpts)
#' 
changepoints.cpt <- function(x, ...) {
  changepoint::cpts(x) |>
    as.integer()
}
