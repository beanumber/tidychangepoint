#' Compatibility layer for wbs
#' @name wbs-generics
#' 
#' @description
#' Methods for generic functions applied to `wbs` objects
#' @param x A `wbs` object returned by [wbs::wbs()]
#' @param ... arguments passed to methods
#' @export
#' @seealso [wbs::wbs-package]
#' @examples
#' cpts <- segment(DataCPSim, method = "wbs")
#' x <- as.segmenter(cpts)
#' class(x)
#' as.seg_cpt(x)
#' as.ts(x) 
#' changepoints(x)
#' fitness(x)
#' model_name(x)
#' model_args(x)
#' nobs(x)
#' seg_params(x)
#' 
as.seg_cpt.wbs <- function(object, ...) {
  seg_cpt(
    x = as.ts(object),
    pkg = "wbs",
    algorithm = "Wild BinSeg",
    changepoints = changepoints(object),
    seg_params = list(seg_params(object)),
    model = model_name(object),
    fitness = fitness(object)
  )
}

#' @rdname as.ts.tidycpt
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "wbs")
#' as.ts(cpts)
#' 
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
#' # Retrive its fitness
#' fitness(x)
#' 
fitness.wbs <- function(object, ...) {
  out <- 0
  names(out) <- "MBIC"
  out
}

#' @rdname wbs-generics
#' @param object A `cpt` object.
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "wbs")
#' nobs(cpts$segmenter)
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
seg_params.wbs <- function(x, ...) {
  list(
    M = x$M,
    integrated = x$integrated,
    rand_intervals = x$rand.intervals,
    threshold = x$cpt$th,
    Kmax = x$cpt$Kmax,
    sigma = x$cpt$sigma
  )
}