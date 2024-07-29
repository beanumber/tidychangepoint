#' Compatibility layer for changepoint
#' @name cpt-generics
#' 
#' @description
#' Methods for generic functions applied to `cpt` objects
#' 
#' @param x A `cpt` object returned by [changepoint::cpt.meanvar()]
#' @param object A `cpt` object.
#' @param ... arguments passed to methods
#' @seealso [changepoint::changepoint-package]
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "pelt")
#' x <- as.segmenter(cpts)
#' class(x)
#' as.seg_cpt(x)
#' as.ts(x) 
#' changepoints(x)
#' fitness(x)
#' logLik(x)
#' model_name(x)
#' model_args(x)
#' nobs(x)
#' seg_params(x)
#' 
as.seg_cpt.cpt <- function(object, ...) {
  seg_cpt(
    x = as.ts(object),
    pkg = "changepoint",
    algorithm = object@method,
    changepoints = changepoints(object),
    seg_params = list(seg_params(object)),
    model = model_name(object),
    fitness = fitness(object)
  )
}


#' @rdname as.ts.tidycpt
#' @export
as.ts.cpt <- function(x, ...) {
  as.ts(x@data.set)
}

#' @rdname changepoints
#' @export
changepoints.cpt <- function(x, ...) {
  changepoint::cpts(x) |>
    as.integer()
}

#' @rdname fitness
#' @export
#' 
fitness.cpt <- function(object, ...) {
  out <- object@pen.value - 2 * as.double(logLik(object))
  names(out) <- object@pen.type
  out
}

#' @rdname cpt-generics
#' @export
logLik.cpt <- function(object, ...) {
  #  message("intercepting...")
  y <- changepoint::likelihood(object) |>
    suppressWarnings()
  ll <- -y[1] / 2
  attr(ll, "df") <- length(object@cpts)
  attr(ll, "nobs") <- nobs(object)
  attr(ll, "tau") <- changepoints(object)
  attr(ll, "real_params_estimated") <- (length(changepoints(object)) + 1) * 2
  class(ll) <- "logLik"
  return(ll)
}

#' @rdname model_name
#' @export
model_name.cpt <- function(object, ...) {
  if (object@cpttype == "mean and variance") {
    return("meanvar")
  } else {
    return("meanshift_norm")
  }
}

#' @rdname model_args
#' @export
model_args.cpt <- function(object, ...) {
  NULL
}

#' @rdname cpt-generics
#' @param object A `cpt` object.
#' @export
nobs.cpt <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname seg_params
#' @export
#' @examples
#' # Segment a time series using PELT
#' x <- segment(CET, method = "pelt")
#' x |>
#'   as.segmenter() |>
#'   seg_params()
#' 
seg_params.cpt <- function(x, ...) {
  list(
    test_stat = x@test.stat,
    num_cpts_max = x@ncpts.max,
    min_seg_length = x@minseglen
  )
}
