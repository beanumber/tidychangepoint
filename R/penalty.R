#' Penalización MDL
#'
#' @param padded_tau Output from [pad_tau()]
#' @param N The length of the time series?
#' @return regresa la evaluación de la penalización
#'  \deqn{
#'    P_{\theta,\tau} = \sum_{i=1}^{m+1}\dfrac{\ln(\tau_i-\tau_{i-1})}{2}+\ln(m)+\sum_{i=2}^m\ln(\tau_i)
#'  }
#' @export
#' @examples
#' x <- segment(DataCPSim, method = "pelt")
#' tau <- changepoints(x)
#' penalty_mdl(pad_tau(tau, n = length(x)))
#' 

penalty_mdl <- function(padded_tau, N = NULL) {
  tau <- unpad_tau(padded_tau)
  m <- length(tau)
  if (m == 0) {
    return(0)
  }
  num_params <- 2
  if (!is.null(N)) {
    extra_term <- m * log(N)
  } else {
    extra_term <- 0
  }
  (num_params / 2) * 
    sum(log(diff(padded_tau))) + 
    2 * log(m) + 
    2 * sum(log(tail(tau, -1))) +
    # is this last term necessary???
    extra_term
}


