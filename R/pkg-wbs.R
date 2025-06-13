#' @rdname as.segmenter
#' @export
#' 
as.seg_cpt.wbs <- function(object, ...) {
  seg_cpt(
    x = as.ts(object),
    pkg = "wbs",
    base_class = class(object),
    algorithm = "Wild BinSeg",
    changepoints = changepoints(object),
    seg_params = list(seg_params(object)),
    model = model_name(object),
    fitness = fitness(object)
  )
}

#' @rdname reexports
#' @export
as.ts.wbs <- function(x, ...) {
  as.ts(x$x)
}

#' @rdname changepoints
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "wbs")
#' changepoints(cpts$segmenter)
#' 
changepoints.wbs <- function(x, ...) {
  x$cpt$cpt.ic$mbic.penalty |>
    sort() |>
    as.integer()
}

#' @rdname fitness
#' @export
#' @examples
#' # Segment a time series using Wild Binary Segmentation
#' x <- segment(DataCPSim, method = "wbs")
#' 
#' # Retrieve its fitness
#' fitness(x)
#' 
fitness.wbs <- function(object, ...) {
  out <- object$cpt$ic.curve$mbic.penalty[1]
  names(out) <- "MBIC"
  out
}

#' @rdname reexports
#' @export
nobs.wbs <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname model_name
#' @export
model_name.wbs <- function(object, ...) {
  "meanshift_norm"
}

#' @rdname model_args
#' @export
model_args.wbs <- function(object, ...) {
  NULL
}

#' @rdname seg_params
#' @export
seg_params.wbs <- function(object, ...) {
  list(
    M = object$M,
    integrated = object$integrated,
    rand_intervals = object$rand.intervals,
    threshold = object$cpt$th,
    Kmax = object$cpt$Kmax,
    sigma = object$cpt$sigma
  )
}