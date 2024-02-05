#' pow
#'
#' @param n_puntos_cambio numero de puntos de cambio
#' @param mas_derecha es una variable que fue utilizada para que
#'  pudiera ejecutarse en R y en JAGS; toma los valores "" (vacío) y "+".
#'
#' @return Regresa el texto correspondiente a la variable m
#' @export
#'

pow <- function(x, y) {
  x^y
}
#' step
#'
#' @param n_puntos_cambio numero de puntos de cambio
#' @param mas_derecha es una variable que fue utilizada para que
#'  pudiera ejecutarse en R y en JAGS; toma los valores "" (vacío) y "+".
#'
#' @return Regresa el texto correspondiente a la variable m
#' @export
#'

step <- function(x) {
  as.numeric(x > 0)
}

#' Calculate overages
#' @export
#' @param x a time series object
#' @examples
#' exceedances(DataCPSim)
exceedances <- function(x) {
  which(x > mean(x))
}

#' @export
pad_tau <- function(tau, n) {
  unique(c(0, tau, n))
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
label_priors <- function() {
  param$mat_phi |>
    as.data.frame() |>
    dplyr::mutate(
      dist = param$vec_dist_a_priori,
      label = paste0(dist, "(", V1, ", ", V2, ")")
    ) |>
    dplyr::pull(label) |>
    paste0(collapse = "_")
}

#' @export
#' @examples
#' split_by_tau(as.ts(lista_AG), changepoints(lista_AG))

split_by_tau <- function(x, tau) {
  idx <- cut_inclusive(1:length(x), pad_tau(tau, length(x)))
  split(x, idx)
}

