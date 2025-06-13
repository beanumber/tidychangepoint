globalVariables(c("median", "q1", "q3"))

#' @rdname as.segmenter
#' @export
as.seg_cpt.ga <- function(object, ...) {
  seg_cpt(
    x = as.ts(object),
    pkg = "GA",
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
as.ts.ga <- function(x, ...) {
  x@data
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

#' @rdname fitness
#' @export
#' @examples
#' # Segment a times series using a genetic algorithm
#' x <- segment(DataCPSim, method = "ga", maxiter = 10)
#' 
#' # Retrieve its fitness value
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

#' @rdname reexports
#' @export
nobs.ga <- function(object, ...) {
  length(as.ts(object))
}

#' Plot GA information
#' @param x A `tidyga` object
#' @param ... currently ignored
#' @returns A [ggplot2::ggplot()] object.
#' @export
#' @examples
#' \donttest{
#' x <- segment(DataCPSim, method = "ga-coen", maxiter = 5)
#' plot(x$segmenter)
#' }
plot.tidyga <- function(x, ...) {
  methods <- c("null", "pelt")
  penalty <- names(fitness(x))
  f <- whomademe(x)
  vals <- methods |>
    purrr::map(~segment(as.ts(x), method = .x)) |>
    purrr::map(changepoints) |>
    purrr::map(~f(as.ts(x), tau = .x)) |>
    purrr::map_dbl(eval(parse(text = penalty)))
  
  guidelines <- tibble::tibble(
    method = c(class(x)[1], methods),
    value = c(fitness(x), vals)
  )
  
  seg <- x@summary |>
    tibble::as_tibble() |>
    dplyr::mutate(
      num_generation = dplyr::row_number()
    )
  
  best <- seg |>
    dplyr::arrange(dplyr::desc(max), num_generation) |>
    utils::head(1)

  ggplot2::ggplot(data = seg, ggplot2::aes(x = num_generation, y = -max)) +
    ggplot2::geom_hline(
      data = guidelines, 
      ggplot2::aes(yintercept = value, color = method), 
      linetype = 2
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        y = -median,
        ymin = -q3, ymax = -q1,
        group = num_generation, width = 0.2
      )
    ) +
    ggplot2::geom_line(color = "blue") +
    ggplot2::geom_point(color = "blue") +
    ggplot2::geom_point(ggplot2::aes(y = -mean), size = 2) +
    ggplot2::geom_vline(xintercept = best$num_generation, linetype = 3) + 
    ggplot2::geom_point(data = best, color = "red") +
#    ggplot2::geom_smooth(se = 0) + 
    ggplot2::scale_y_continuous(penalty) +
    ggplot2::scale_x_continuous("Generation of Candidate Changepoints") +
    ggplot2::labs(
      title = "Evolution of Objective Function values",
      subtitle = "Comparison with other known algorithms"
    )
}



#' @rdname seg_params
#' @export
seg_params.ga <- function(object, ...) {
  list(
    popSize = object@popSize,
    iter = object@iter,
    elitism = object@elitism,
    pcrossover = object@pcrossover,
    pmutation = object@pmutation,
    model_fn_args = model_args(object)
  )
}

#' Initialize populations in genetic algorithms
#' 
#' @description
#' Build an initial population set for genetic algorithms
#' 
#' @inheritParams segment
#' @details
#' Genetic algorithms require a method for randomly generating initial 
#' populations (i.e., a first generation). 
#' The default method used by [GA::ga()] for changepoint detection is usually 
#' [GA::gabin_Population()], which selects candidate changepoints uniformly at
#' random with probability 0.5. 
#' This leads to an initial population with excessively large candidate 
#' changepoint sets (on the order of \eqn{n/2}), which makes the genetic 
#' algorithm slow. 
#'   
#'   - [build_gabin_population()] takes a `ts` object and runs several fast 
#'   changepoint detection algorithms on it, then sets the initial probability
#'   to 3 times the average value of the size of the changepoint sets returned 
#'   by those algorithms. This is a conservative guess as to the likely size of 
#'   the optimal changepoint set. 
#'   - [log_gabin_population()] takes a `ts` object and sets the initial 
#'   probability to the natural logarithm of the length of the time series. 
#' 
#' @return A `function` that can be passed to the `population` argument of
#' [GA::ga()] (through [segment_ga()])
#' @export
#' @seealso [GA::gabin_Population()], [segment_ga()]
#' @examples
#' # Build a function to generate the population
#' f <- build_gabin_population(CET)
#' 
#' # Segment the time series using the population generation function
#' segment(CET, method = "ga", population = f, maxiter = 5)


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