#' Penalización MDL
#'
#' @param cp vector de extendido de puntos de cambio
#'
#' @return regresa la evaluación de la penalización
#'  \deqn{
#'    P_{\theta,\tau} = \sum_{i=1}^{m+1}\dfrac{\ln(\tau_i-\tau_{i-1})}{2}+\ln(m)+\sum_{i=2}^m\ln(\tau_i)
#'  }
#' @export
#' @examples
#' x <- segment(DataCPSim, method = "cpt-pelt")
#' tau <- changepoints(x)
#' penalty_mdl(pad_tau(tau, n = length(x)))
#' 

penalty_mdl <- function(padded_tau, N = NULL) {
  tau <- unpad_tau(padded_tau)
  m <- length(tau)
  if (!is.null(N)) {
    extra_term <- m * log(N)
  } else {
    extra_term <- 0
  }
  2 * sum(log(diff(padded_tau)) / 2) + 
    log(m) + 
    sum(log(tau)) +
    # is this last term necessary???
    extra_term
}


