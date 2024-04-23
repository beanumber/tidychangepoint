#' Compatibility layer for GA
#' @name ga-generics
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
    num_cpts = length(changepoints(x)),
    model = model_name(x),
    criteria = names(fitness(x)),
    fitness = fitness(x)
  )
}

#' @rdname ga-generics
#' @export
params.ga <- function(x, ...) {
  list(
    popSize = x@popSize,
    iter = x@iter,
    elitism = x@elitism,
    pcrossover = x@pcrossover,
    pmutation = x@pmutation,
    model_fn_args = model_args(x)
  )
}

#' @rdname ga-generics
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 5)
#' as.ts(cpts)
#' 
as.ts.ga <- function(x, ...) {
  x@data
}

#' @rdname ga-generics
#' @param object A `ga` object.
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 5)
#' nobs(cpts)
#' 
nobs.ga <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname changepoints
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 5)
#' changepoints(cpts$segmenter)
#' 
changepoints.ga <- function(x, ...) {
  which(x@solution[1, ] == 1)
}

#' Build an initial population set for GA algorithms
#' @inheritParams segment
#' @export
#' @seealso [GA::gabin_Population()]
#' @examples
#' f <- build_gabin_population(CET)
#' segment(CET, method = "ga", population = f)

build_gabin_population <- function(x, ...) {
  p <- list(
    segment(x, method = "pelt"),
    segment(x, method = "binseg"),
    segment(x, method = "wbs")
  ) |>
    purrr::map(changepoints) |>
    purrr::map_int(length) |>
    mean() * 3 / length(x)
  
  f <- function(object, ...) {
    message(paste("Seeding initial population with probability:", p))
    stats::rbinom(object@nBits * object@popSize, size = 1, prob = p) |>
      matrix(ncol = object@nBits)
  }
  return(f)
}

#' @rdname build_gabin_population
#' @export
#' @examples
#' f <- log_gabin_population(CET)
#' segment(CET, method = "ga", population = f, maxiter = 10)

log_gabin_population <- function(x, ...) {
  p <- log(length(x)) / length(x)
  
  f <- function(object, ...) {
    message(paste("Seeding initial population with probability:", p))
    stats::rbinom(object@nBits * object@popSize, size = 1, prob = p) |>
      matrix(ncol = object@nBits)
  }
  return(f)
}

#' @rdname fitness
#' @export
#' @examples
#' x <- segment(DataCPSim, method = "ga", maxiter = 10)
#' fitness(x)
#' 
fitness.ga <- function(object, ...) {
  out <- -object@fitnessValue
  names(out) <- model_args(object)[["penalty_fn"]]
  out
}

#' @rdname model_name
#' @export
model_name.ga <- function(object, ...) {
  model_args(object)[["model_fn"]]
}

#' @rdname model_args
#' @export
model_args.ga <- function(object, ...) {
  object@model_fn_args
}