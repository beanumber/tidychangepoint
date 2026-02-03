#' @rdname as.segmenter
#' @export
#' 
as.seg_cpt.segmented <- function(object, ...) {
  seg_cpt(
    x = as.ts(object),
    pkg = "segmented",
    base_class = class(object),
    algorithm = "selgmented",
    changepoints = changepoints(object),
    seg_params = list(seg_params(object)),
    model = model_name(object),
    fitness = fitness(object)
  )
}

#' @rdname as.segmenter
#' @export
#' 
as.seg_cpt.stepmented <- function(object, ...) {
  seg_cpt(
    x = as.ts(object),
    pkg = "segmented",
    base_class = class(object),
    algorithm = "stepmented",
    changepoints = changepoints(object),
    seg_params = list(seg_params(object)),
    model = model_name(object),
    fitness = fitness(object)
  )
}

#' @rdname reexports
#' @export
as.ts.segmented <- function(x, ...) {
  stats::model.frame(x)[, 1] |>
    as.ts()
}

#' @rdname reexports
#' @export
as.ts.stepmented <- function(x, ...) {
  # where did the original data go???
  fitted(x) + residuals(x) |>
    as.ts()
}


#' @rdname changepoints
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "selgmented")
#' changepoints(cpts$segmenter)
#' 
changepoints.segmented <- function(x, ...) {
  x$psi[ , 2] |>
    round() |>
    as.integer()
}

#' @rdname changepoints
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "stepmented")
#' changepoints(cpts$segmenter)
#' 
changepoints.stepmented <- function(x, ...) {
  x$psi[[1, 1]] |>
    round() |>
    as.integer()
}

#' @rdname fitness
#' @export
#' @examples
#' # Segment a time series using Segmented
#' x <- segment(DataCPSim, method = "selgmented")
#' 
#' # Retrieve its fitness
#' fitness(x)
#' 
fitness.lm <- function(object, ...) {
  out <- BIC(object)
  names(out) <- "BIC"
  out
}

#' @rdname model_name
#' @export
model_name.segmented <- function(object, ...) {
  "trendshift"
}

#' @rdname model_name
#' @export
model_name.stepmented <- function(object, ...) {
  "meanshift"
}

#' @rdname model_args
#' @export
model_args.lm <- function(object, ...) {
  NULL
}

#' @rdname seg_params
#' @export
seg_params.lm <- function(object, ...) {
  list(
    sigma = sqrt(sum(residuals(object)^2))
  )
}
