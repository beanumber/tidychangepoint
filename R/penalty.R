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

penalty_mdl <- function(padded_tau) {
  tau <- unpad_tau(padded_tau)
  m <- length(tau)
  2 * sum(log(diff(padded_tau)) / 2) + 
    log(m) + 
    sum(log(tau))
  # + m * log(m)????
}


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
#' mat_cp <- sim_k_cp_BMDL(DataCPSim)
#' penalization_MDL(mat_cp, N = max(exceedances(DataCPSim)))
#' 
#'
penalization_MDL <- function(cp, nhpp_dist = "W", N) { # V02
  # Se hizo el cambio de multiplicar por en número de parámetros
  # esta función solo es llamada por "penalization_MDL"
  # penalization_MDL <- function(cp) { # antes no recibía nhpp_dist
  
  # n_param_nhpp_dist es el número de parámetros de la función de tasa del poisson
  n_param_nhpp_dist <- c(2, 3, 3, 2, 2)[nhpp_dist == c("W", "EW", "GGO", "MO", "GO")]
  
  (cp_corto_cero <- c(0, cp[3:(cp[1] + 3)]))
  # En particular se agregó la parte *n_param_nhpp_dist/2
  return(sum(log(cp_corto_cero[-1] - cp_corto_cero[-cp[1] - 2]) * n_param_nhpp_dist / 2) + log(cp[1]) + sum(log(cp_corto_cero[c(-1, -cp[1] - 2)])) + (cp[1] * log(N)))
}

