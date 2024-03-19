#' Compatibility layer for GA
#' @param x A `GA` object returned by [GA::ga()]
#' @param ... arguments passed to methods
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 10)
#' class(cpts)
#' y <- augment(cpts)
#' class(y)
#' y
#' tidy(cpts)
#' glance(cpts)

glance.ga <- function(x, ...) {
  tibble::tibble(
    pkg = "ga",
    version = package_version(utils::packageVersion("GA")),
    algorithm = "Genetic",
    params = list(params(x)),
    num_cpts = length(changepoints(x))
  )
}

#' @rdname glance.ga
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

#' @rdname glance.ga
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 5)
#' as.ts(cpts)
#' 
as.ts.ga <- function(x, ...) {
  x@data
}

#' @rdname glance.ga
#' @param object A `ga` object.
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 5)
#' nobs(cpts)
#' 
nobs.ga <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname glance.ga
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 5)
#' changepoints(cpts$segmenter)
#' 
changepoints.ga <- function(x, ...) {
  which(x@solution[1, ] == 1)
}
