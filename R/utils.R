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
