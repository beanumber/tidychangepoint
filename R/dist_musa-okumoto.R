#' Evaluanción de rate function (la derivada de la mean)
#'
#' @param t valor a evaluar (real)
#' @param nhpp_dist tipo de rate function (ver articulo A Gibbs Sampling Algorithm
#'   to Estimate the Occurrence of Ozone Exceedances in Mexico City  para el
#'   caso de Weibull (W) y ver libro de Eliane pdf 40 eq (3.12)); posibles
#'   exponentiated-Weibull (EW),the Musa–Okumoto (MO), the Goel–Okumoto (GO),
#'   and a generalized Goel–Okumoto (GGO). Las cuales tienen 3,3,2 y 2
#'   parámetros
#' @param theta parámetros de mean function. Para
#'
#'  - W: \eqn{\alpha = \theta[1]}, \eqn{\sigma = \theta[2]}
#'  - EW: \eqn{\alpha = \theta[1], \beta = \theta[2]} y \eqn{\sigma = \theta[3]}
#'  - GGO: \eqn{\alpha = \theta[1], \beta = \theta[2]} y \eqn{\gamma = \theta[3]}
#'  - MO: \eqn{\alpha = \theta[1], \beta = \theta[2]}
#'  - GO: \eqn{\alpha = \theta[1], \beta = \theta[2]}
#'
#' @return regresa la evaluación
#' @export
#'
#' @examples
#' t <- c(1.4, 2.8)
#' theta <- c(1.2, 2.1, 3.2)
#'
#' imusaokomoto(t, theta)
#'
imusaokomoto <- function(t, theta) {
  # MO -> theta = c(alpha,beta)
  theta[2] / (t + theta[1])
}


#' @rdname imusaokomoto
#' @param t valor a evaluar (real)
#' @param nhpp_dist tipo de mean function (ver libro de Eliane pdf 40 eq (3.12));
#'               posibles exponentiated-Weibull (EW),the Musa–Okumoto (MO),
#'               the Goel–Okumoto (GO), and a generalized Goel–Okumoto (GGO)
#' @param theta parámetros de mean function.Para
#'
#'  - W: \eqn{\alpha = \theta[1], \sigma = \theta[2]}
#'  - EW: \eqn{\alpha= \theta[1], \beta = \theta[2]} y \eqn{\sigma = \theta[3]}
#'  - GGO: \eqn{\alpha = \theta[1], \beta = \theta[2]} y \eqn{\gamma = \theta[3]}
#'  - MO: \eqn{\alpha = \theta[1], \beta = \theta[2]}
#'  - GO: \eqn{\alpha = \theta[1], \beta = \theta[2]}
#'
#' @return regresa la evaluación
#' @export
#'
#' @examples
#' t <- c(1.4, 2.8)
#' theta <- c(1.2, 2.1, 3.2)
#' mmusaokomoto(t, theta)
#'
mmusaokomoto <- function(t, theta) {
  theta[2] * log(1 + t / theta[1])
}


#' @rdname imusaokomoto
#' @export
#' @examples
#' log_likelihood_region_musaokomoto(DataCPSim, 0, 575, theta = c(0.5, 0.5))
#' 
log_likelihood_region_musaokomoto <- function(t, tau_left, tau_right, theta) {
  (tau_left^theta[1] - tau_right^theta[1]) / theta[2]^theta[1] +
    length(t) * (log(theta[1]) - theta[1] * log(theta[2])) +
    (theta[1] - 1) * sum(log(t))
}

#' @rdname imusaokomoto
#' @export
#' @examples
#' hyper <- data.frame(
#'   hyperprior_shape = c(1, 3),
#'   hyperprior_scale = c(2, 1.2)
#' )
#' log_prior_region_musaokomoto(theta = c(0.5, 0.5), hyper)
log_prior_region_musaokomoto <- function(theta, hyperparameters) {
  x <- hyperparameters$hyperprior_shape
  y <- hyperparameters$hyperprior_scale
  
  (y[1] - 1) * log(theta[1]) - x[1] * theta[1] + # Exp 74 pag 21
    (y[2] - 1) * log(theta[2]) - x[2] * theta[2] # Exp 75 pag 21
}


#' @rdname imusaokomoto
#' @param vec_dist_a_priori vector que determina cual es la distribución a
#'   priori de los parametros; por ahora se tiene programado
#'   vec_dist_a_priori=c("Gamma","Gamma") y
#'   vec_dist_a_priori=c("Gamma","Gamma","Gamma")
#' @param theta vector de parámetros de verosimilitud
#' @param mat_phi matriz cuyos renglones tiene los parámetros de las
#'   distribuciones a priori; cada renglón tiene todos los parametros de una
#'   distribución
#'
#' @export
D_log_prior_region_musaokomoto <- function(theta, hyperparameters) {
  x <- hyperparameters$hyperprior_shape
  y <- hyperparameters$hyperprior_scale
  # Parcial con respecto a alfa
  p1 <- (-1 - theta[1] * x[1] + y[1]) / theta[1]
  # Parcial con respecto a beta
  p2 <- (-1 - theta[2] * x[2] + y[2]) / theta[2]
  return(c(p1, p2))
}

#' @rdname imusaokomoto
#' @param vec_d_i vector de días en los que hubo revases entre el los puntos de
#'   cambio tau1 y tau2
#' @param tau1 valor del primer punto de cambio
#' @param tau2 valor del segundo punto de cambio
#' @param nhpp_dist nombre de tasa de NHPP
#' @param theta vector de parámetros de verosimilitud del NHPP
#' @export
D_log_likelihood_region_musaokomoto <- function(vec_d_i, tau1, tau2, theta, nhpp_dist = "W") {
  if (nhpp_dist == "W") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    sumlogd <- sum(log(vec_d_i))
    if (tau1 == 0) { # este es el caso del primer regimen
      # Parcial de alpha
      p1 <- difN / alpha + sumlogd - difN * log(beta) + beta^(-alpha) * tau2^alpha * (log(beta) - log(tau2))
      # Parcial de beta
      p2 <- alpha * beta^(-1 - alpha) * (-difN * beta^alpha + tau2^alpha)
    } else { # para los otros regímenes (o bloques)
      # Parcial de alpha
      p1 <- difN / alpha + sumlogd + beta^(-alpha) * (-(difN * beta^alpha + tau1^alpha - tau2^alpha) * log(beta) + tau1^alpha * log(tau1) - tau2^alpha * log(tau2))
      # Parcial de beta
      p2 <- -alpha * beta^(-1 - alpha) * (difN * beta^alpha + tau1^alpha - tau2^alpha)
    }
  }
}
