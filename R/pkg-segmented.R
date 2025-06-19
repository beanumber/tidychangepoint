#' @rdname as.segmenter
#' @export
#' 
as.seg_cpt.segmented <- function(object, ...) {
  seg_cpt(
    x = as.ts(object),
    pkg = "segmented",
    base_class = class(object),
    algorithm = "Segmented",
    changepoints = changepoints(object),
    seg_params = list(seg_params(object)),
    model = model_name(object),
    fitness = fitness(object)
  )
}

#' @rdname reexports
#' @export
as.ts.segmented <- function(x, ...) {
  as.ts(x$model$obj)
}

#' @rdname changepoints
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "segmented")
#' changepoints(cpts$segmenter)
#' 
changepoints.segmented <- function(x, ...) {
  x$psi[ , 2] |>
    round() |>
    as.integer()
}

#' @rdname fitness
#' @export
#' @examples
#' # Segment a time series using Segmented
#' x <- segment(DataCPSim, method = "segmented")
#' 
#' # Retrieve its fitness
#' fitness(x)
#' 
fitness.segmented <- function(object, ...) {
  out <- MDL(object)
  names(out) <- "MDL"
  out
}

#' @rdname reexports
#' @export
nobs.segmented <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname model_name
#' @export
model_name.segmented <- function(object, ...) {
  "trendshift"
}

#' @rdname model_args
#' @export
model_args.segmented <- function(object, ...) {
  NULL
}

#' @rdname seg_params
#' @export
seg_params.segmented <- function(object, ...) {
  list(
    sigma = sqrt(sum(residuals(object)^2))
  )
}