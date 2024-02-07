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


#' Bayesian MDL para un vector de puntos de cambio
#'
#' @param cp description
#' @param x description
#' @export
#' @examples
#' bmdl(DataCPSim, tau = 826)
#' bmdl(DataCPSim, tau = changepoints(segment(DataCPSim, method = "cpt-pelt")))
#' bmdl(DataCPSim, tau = changepoints(segment(DataCPSim, method = "single-best")))
#' 

bmdl <- function(x, tau) {
  # 1. Obtener los estimadores MAP para cada regimen y guardarlos en mat_MAP
  theta <- fit_nhpp(x, tau)
  # 2. Evaluar la log-posterior (sumando la primera columna de mat_MAP)
  log_posterior <- sum(theta$log_posterior)
  # 3. Evaluar la penalización
  penalty <- penalty_mdl(pad_tau(tau, n = length(x)))
  # 4. Obtener bayesian-MDL de la diferencia de la penalización y la log-posterior
  penalty - log_posterior
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
#' penalization_MDL(mat_cp, param$rf_type, N = max(exceedances(DataCPSim)))
#' 
#'
penalization_MDL <- function(cp, rf_type, N) { # V02
  # Se hizo el cambio de multiplicar por en número de parámetros
  # esta función solo es llamada por "penalization_MDL"
  # penalization_MDL <- function(cp) { # antes no recibía rf_type
  
  # n_param_rf_type es el número de parámetros de la función de tasa del poisson
  n_param_rf_type <- c(2, 3, 3, 2, 2)[rf_type == c("W", "EW", "GGO", "MO", "GO")]
  
  (cp_corto_cero <- c(0, cp[3:(cp[1] + 3)]))
  # En particular se agregó la parte *n_param_rf_type/2
  return(sum(log(cp_corto_cero[-1] - cp_corto_cero[-cp[1] - 2]) * n_param_rf_type / 2) + log(cp[1]) + sum(log(cp_corto_cero[c(-1, -cp[1] - 2)])) + (cp[1] * log(N)))
}

