#' Compatibility layer for wbs
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
    num_cpts = length(changepoints(x))
  )
}

#' @rdname glance.cpt
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

#' @rdname glance.cpt
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "wbs")
#' as.ts(cpts)
#' 
as.ts.wbs <- function(x, ...) {
  as.ts(x$x)
}

#' @rdname glance.cpt
#' @param object A `cpt` object.
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "wbs")
#' nobs(cpts)
#' 
nobs.wbs <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname glance.cpt
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "wbs")
#' logLik(cpts)
#' 
logLik.wbs <- function(object, ...) {
#  message("intercepting...")
  y <- 0
  attr(ll, "df") <- length(changepoints(object))
  class(ll) <- "logLik"
  return(ll)
}

#' @rdname glance.cpt
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
