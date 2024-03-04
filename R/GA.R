#' Compatibility layer for GA
#' @param x A `GA` object returned by [GA::ga()]
#' @param ... arguments passed to methods
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 20)
#' class(cpts)
#' y <- augment(cpts)
#' class(y)
#' y
#' tidy(cpts)
#' glance(cpts)

glance.ga <- function(x, ...) {
  tibble::tibble(
    pkg = "ga",
    version = package_version(packageVersion("GA")),
    algorithm = "Genetic",
    params = list(params(x)),
    num_cpts = length(changepoints(x))
  )
}

#' @rdname glance.cpt
#' @export
params.ga <- function(x, ...) {
  list(
    popSize = x@popSize,
    iter = x@iter,
    elitism = x@elitism,
    pcrossover = x@pcrossover,
    pmutation = x@pmutation
  )
}

#' @rdname glance.cpt
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 20)
#' as.ts(cpts)
#' 
as.ts.ga <- function(x, ...) {
  x@data
}

#' @rdname glance.cpt
#' @param object A `cpt` object.
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 20)
#' nobs(cpts)
#' 
nobs.ga <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname glance.cpt
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga")
#' logLik(cpts)
#' 
logLik.ga <- function(object, ...) {
#  message("intercepting...")
  y <- 0
  attr(ll, "df") <- length(changepoints(object))
  class(ll) <- "logLik"
  return(ll)
}

#' @rdname glance.cpt
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 20)
#' changepoints(cpts$segmenter)
#' 
changepoints.ga <- function(x, ...) {
  which(x@solution == 1)
}
