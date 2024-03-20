#' Compatibility layer for wbs
#' @name wbs-generics
#' @param x A `wbs` object returned by [wbs::wbs()]
#' @param ... arguments passed to methods
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "wbs")
#' class(cpts)
#' y <- augment(cpts)
#' class(y)
#' y
#' tidy(cpts)
#' glance(cpts)

glance.wbs <- function(x, ...) {
  tibble::tibble(
    pkg = "wbs",
    version = package_version(utils::packageVersion("wbs")),
    algorithm = "Wild Binary Segmentation",
    params = list(params(x)),
    num_cpts = length(changepoints(x)),
    sigma = x$cpt$sigma,
#    BIC = min(x$cpt$ic.curve$bic.penalty),
#    MBIC = min(x$cpt$ic.curve$mbic.penalty)
  )
}

#' @rdname wbs-generics
#' @export
params.wbs <- function(x, ...) {
  list(
    M = x$M,
    integrated = x$integrated,
    rand_intervals = x$rand.intervals,
    threshold = x$cpt$th,
    Kmax = x$cpt$Kmax
  )
}

#' @rdname wbs-generics
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "wbs")
#' as.ts(cpts)
#' 
as.ts.wbs <- function(x, ...) {
  as.ts(x$x)
}

#' @rdname wbs-generics
#' @param object A `cpt` object.
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "wbs")
#' nobs(cpts)
#' 
nobs.wbs <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname wbs-generics
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "wbs")
#' logLik(cpts)
#' 
logLik.wbs <- function(object, ...) {
  NA
}

#' @rdname changepoints
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "wbs")
#' changepoints(cpts$segmenter)
#' 
changepoints.wbs <- function(x, ...) {
  x$cpt$cpt.th[[1]] |>
    sort() |>
    as.integer()
}
