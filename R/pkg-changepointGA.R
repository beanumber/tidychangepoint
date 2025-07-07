#' @rdname as.segmenter
#' @export
as.seg_cpt.cga <- function(object, ...) {
  seg_cpt(
    x = as.ts(object),
    pkg = "changepointGA",
    base_class = class(object),
    algorithm = "Genetic",
    changepoints = changepoints(object),
    seg_params = list(seg_params(object)),
    model = model_name(object),
    fitness = fitness(object)
  )
}

#' @rdname reexports
#' @export
as.ts.cga <- function(x, ...) {
  x$data
}

#' @rdname changepoints
#' @export
#' @examples
#' \donttest{
#' # Segment a times series using a genetic algorithm
#' cpts <- segment(DataCPSim, method = "cga")
#' changepoints(cpts$segmenter)
#' }
changepoints.cga <- function(x, ...) {
  x$overbestchrom |>
    utils::head(-1) |>
    utils::tail(-1) |>
    as.integer()
}

#' @rdname fitness
#' @export
#' @examples
#' \donttest{
#' # Segment a times series using a genetic algorithm
#' x <- segment(DataCPSim, method = "cga")
#' 
#' # Retrieve its fitness value
#' fitness(x)
#' }
fitness.cga <- function(object, ...) {
  out <- object$overbestfit
  names(out) <- "BIC"
  out
}

#' @rdname model_name
#' @export
model_name.cga <- function(object, ...) {
  "arima"
}

#' @rdname reexports
#' @export
nobs.cga <- function(object, ...) {
  length(as.ts(object))
}


#' @rdname seg_params
#' @export
seg_params.cga <- function(object, ...) {
  list()
}
