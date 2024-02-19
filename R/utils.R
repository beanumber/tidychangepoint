#' Calculate overages
#' @export
#' @param x a time series object
#' @examples
#' exceedances(DataCPSim)
exceedances <- function(x) {
  y <- as.ts(x)
  which(y > mean(y))
}

#' @export
pad_tau <- function(tau, n) {
  unique(c(0, tau, n))
}

#' @export
unpad_tau <- function(padded_tau) {
  padded_tau |>
    utils::head(-1) |>
    utils::tail(-1)
}

#' @export
cut_inclusive <- function(x, tau) {
  cut(x, breaks = tau, include.lowest = TRUE, right = FALSE)
}

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

#' Genera los textos para los pdf y RData de las n distribuciones
#' @export
label_priors <- function(x) {
  x$mat_phi |>
    as.data.frame() |>
    dplyr::mutate(
      dist = x$vec_dist_a_priori,
      label = paste0(dist, "(", V1, ", ", V2, ")")
    ) |>
    dplyr::pull(label) |>
    paste0(collapse = "_")
}

#' @rdname exceedances
#' @export
#' @examples
#' split_by_tau(as.ts(lista_AG), changepoints(lista_AG))

split_by_tau <- function(x, tau) {
  idx <- cut_inclusive(1:length(x), pad_tau(tau, length(x)))
  split(x, idx)
}

#' @rdname exceedances
#' @export
#' @examples
#' deg_free(segment(DataCPSim))

deg_free <- function(x, ...) {
  attr(logLik(x), "df")
}

