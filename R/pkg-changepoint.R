#' @rdname as.segmenter
#' @export

as.seg_cpt.cpt <- function(object, ...) {
  seg_cpt(
    x = as.ts(object),
    pkg = "changepoint",
    base_class = class(object),
    algorithm = object@method,
    changepoints = changepoints(object),
    seg_params = list(seg_params(object)),
    model = model_name(object),
    fitness = fitness(object)
  )
}


#' @rdname reexports
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

#' @rdname reexports
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

#' @rdname reexports
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
seg_params.cpt <- function(object, ...) {
  list(
    test_stat = object@test.stat,
    num_cpts_max = object@ncpts.max,
    min_seg_length = object@minseglen
  )
}
