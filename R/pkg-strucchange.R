#' @rdname as.segmenter
#' @export
#' 
as.seg_cpt.breakpointsfull <- function(object, ...) {
  seg_cpt(
    x = as.ts(object),
    pkg = "strucchange",
    base_class = class(object),
    algorithm = "Bellman DP",
    changepoints = changepoints(object),
    seg_params = list(seg_params(object)),
    model = model_name(object),
    fitness = fitness(object)
  )
}

#' @rdname reexports
#' @export
as.ts.breakpointsfull <- function(x, ...) {
  as.ts(x$y)
}

#' @rdname changepoints
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "strucchange")
#' changepoints(cpts$segmenter)
#' 
changepoints.breakpointsfull <- function(x, ...) {
  x$breakpoints |>
    as.integer()
}

#' @rdname fitness
#' @export
#' @examples
#' \donttest{
#' # Segment a time series using Segmented
#' x <- segment(DataCPSim, method = "strucchange")
#' 
#' # Retrieve its fitness
#' fitness(x)
#' }
fitness.breakpointsfull <- function(object, ...) {
  out <- strucchange::breakpoints(object)[["RSS"]]
  names(out) <- "RSS"
  out
}

#' @rdname reexports
#' @export
nobs.breakpointsfull <- function(object, ...) {
  object$nobs
}

#' @rdname model_name
#' @export
model_name.breakpointsfull <- function(object, ...) {
  "lmshift"
}

#' @rdname model_args
#' @export
model_args.breakpointsfull <- function(object, ...) {
  NULL
}

#' @rdname seg_params
#' @export
seg_params.breakpointsfull <- function(object, ...) {
  list(
    sigma = sqrt(sum(residuals(object)^2))
  )
}