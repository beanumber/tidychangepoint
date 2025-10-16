#' @rdname as.segmenter
#' @export
as.seg_cpt.cptga <- function(object, ...) {
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
as.ts.cptga <- function(x, ...) {
  x@data
}

#' @rdname changepoints
#' @export
#' @examples
#' \donttest{
#' # Segment a times series using a genetic algorithm
#' cpts <- segment(DataCPSim, method = "cptga")
#' changepoints(cpts$segmenter)
#' }
changepoints.cptga <- function(x, ...) {
  x@overbestchrom |>
    utils::head(-1) |>
    utils::tail(-1) |>
    as.integer()
}

#' @rdname fitness
#' @export
#' @examples
#' \donttest{
#' # Segment a times series using a genetic algorithm
#' x <- segment(DataCPSim, method = "cptga")
#' 
#' # Retrieve its fitness value
#' fitness(x)
#' }
fitness.cptga <- function(object, ...) {
  out <- object@overbestfit
  names(out) <- "BIC"
  out
}

#' @rdname model_args
#' @export
model_args.cptga <- function(object, ...) {
  object@model_fn_args
}

#' @rdname model_name
#' @export
model_name.cptga <- function(object, ...) {
  "arima"
}

#' @rdname reexports
#' @export
nobs.cptga <- function(object, ...) {
  length(as.ts(object))
}


#' @rdname seg_params
#' @export
seg_params.cptga <- function(object, ...) {
  list(
    popSize = object@popSize,
    minDist = object@minDist,
    pchangepoint = object@pchangepoint,
    pcrossover = object@pcrossover,
    pmutation = object@pmutation,
    mmax = object@mmax,
    lmax = object@lmax,
    maxgen = object@maxgen,
    maxconv = object@maxconv
  )
}
