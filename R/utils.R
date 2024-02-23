#' Utility functions
#' @param tau a numeric vector of changepoints
#' @param n the length of the original time series
#' @export
pad_tau <- function(tau, n) {
  unique(c(0, tau, n))
}

#' @rdname pad_tau
#' @param padded_tau Output from [pad_tau()]
#' @export
unpad_tau <- function(padded_tau) {
  padded_tau |>
    utils::head(-1) |>
    utils::tail(-1)
}

#' @rdname pad_tau
#' @param x A numeric vector
#' @export
cut_inclusive <- function(x, tau) {
  cut(x, breaks = tau, include.lowest = TRUE, right = FALSE)
}

#' @rdname pad_tau
#' @param n Number of changepoints
#' @param sd Standard deviation passed to [stats::rnorm()]
#' @param seed Value passed to [base::set.seed()]
#' @export
test_set <- function(n = 1, sd = 1, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  num_obs <- 1000
  tau <- sample.int(n = num_obs, size = n) |>
    sort()
  means <- sample.int(n = 100, size = n + 1)
  
  region_lengths <- tau |>
    pad_tau(num_obs) |>
    diff()
  
  out <- purrr::map2(region_lengths, means, ~rnorm(.x, mean = .y, sd = sd)) |>
    c(recursive = TRUE) |>
    as.ts()
  attr(out, "cpt_true") <- tau
  return(out)
}

#' @rdname pad_tau
#' @export
#' @examples
#' split_by_tau(as.ts(lista_AG), changepoints(lista_AG))

split_by_tau <- function(x, tau) {
  idx <- cut_inclusive(1:length(x), pad_tau(tau, length(x)))
  split(x, idx)
}

#' @rdname pad_tau
#' @export
#' @examples
#' deg_free(segment(DataCPSim))

deg_free <- function(x) {
  attr(logLik(x), "df")
}

