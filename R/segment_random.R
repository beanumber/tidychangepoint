#' Segment a times series using randomly selected candidate changepoints
#' @param x A times series
#' @param num_generations Number of generations
#' @param ... currently ignored
#' @export
#' @examples
#' segment_random(DataCPSim, 10)
#' 
segment_random <- function(x, num_generations = 10, ...) {
  cpts <- random_cpts(x, n = num_generations)
  new_seg_default(x, cpt_list = cpts, ...)
}

#' @rdname segment_random
#' @param n Number of random changepoints sets to generate
#' @param k Rate function for Poisson distribution of mean changepoint set length. 
#' @export
#' @examples
#' random_cpts(DataCPSim, n = 5)
#' 
random_cpts <- function(x, n = 10, k = round(log(length(as.ts(x)))), ...) {
  sizes <- stats::rpois(n, lambda = k)
  sizes |>
    purrr::map(~sort(sample.int(n = length(as.ts(x)) - 1, size = .x)))
}
