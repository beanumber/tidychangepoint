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
#' iweibull(1, shape = 1, scale = 1)
#' plot(x = 1:10, y = iweibull(1:10, shape = 2, scale = 2))

iweibull <- function(x, shape, scale = 1) {
  (shape / scale) * (x / scale)^(shape - 1)
}


#' Evaluanción de mean function (la integral de la rate)
#'
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
#' mweibull(1, shape = 1, scale = 1)
#' plot(x = 1:10, y = mweibull(1:10, shape = 1, scale = 1))
#' plot(x = 1:10, y = mweibull(1:10, shape = 1, scale = 2))
#' plot(x = 1:10, y = mweibull(1:10, shape = 0.5, scale = 2))
#' plot(x = 1:10, y = mweibull(1:10, shape = 0.5, scale = 100))
#' plot(x = 1:10, y = mweibull(1:10, shape = 2, scale = 2))
#' plot(x = 1:10, y = mweibull(1:10, shape = 2, scale = 100))
#'
mweibull <- function(x, shape, scale = 1) {
  (x / scale)^shape
}

#' @export

parameters_weibull <- function(...) {
  list(
    shape = list(
      dist = "gamma",
      shape = 1,
      rate = 2,
      initial_value = 0.1,
      lower_bound = 0.0001,
      upper_bound = 1
    ),
    scale = list(
      dist = "gamma",
      shape = 3,
      rate = 1.2,
      initial_value = 0.5,
      lower_bound = 1e-8,
      upper_bound = 100000
    )
  )
}

#' Log-likelihood
#' @export
#' @examples
#' log_likelihood_region_weibull(DataCPSim, 0, 575, theta = c(0.5, 2))
#' 
log_likelihood_region_weibull <- function(t, tau_left, tau_right, theta) {
  (tau_left^theta[1] - tau_right^theta[1]) / theta[2]^theta[1] +
    length(t) * (log(theta[1]) - theta[1] * log(theta[2])) +
    (theta[1] - 1) * sum(log(t))
}

#' @export
#' @examples
#' log_prior_region_weibull(theta = c(0.5, 2))
log_prior_region_weibull <- function(theta, params = parameters_weibull()) {
  # a.k.a. the shape parameter for the Weibull distribution
  alpha <- hyperparameters[["shape"]]
  # a.k.a. the scale parameter for the Weibull distribution
  beta <- hyperparameters[["scale"]]

  (alpha$rate - 1) * log(theta[1]) - alpha$shape * theta[1] + # Exp 74 pag 21
    (beta$rate - 1) * log(theta[2]) - beta$shape * theta[2] # Exp 75 pag 21
}

#' Derivada de bloque de log-a priori NHPP
#' @rdname D_Bloq_LogPost_NHPP
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
#' @examples
#' D_log_prior_region_weibull(theta = c(0.5, 2))
D_log_prior_region_weibull <- function(theta, params = parameters_weibull()) {
  shape <- hyperparameters[["shape"]]
  scale <- hyperparameters[["scale"]]
  # Parcial con respecto a alfa
  p1 <- (-1 - theta[1] * shape$shape + scale$shape) / theta[1]
  # Parcial con respecto a beta
  p2 <- (-1 - theta[2] * shape$rate + scale$rate) / theta[2]
  return(c(p1, p2))
}

#' Derivadas de bloque de log-verosimilitud NHPP
#' @rdname D_Bloq_LogPost_NHPP
#' @param vec_d_i vector de días en los que hubo revases entre el los puntos de
#'   cambio tau1 y tau2
#' @param tau1 valor del primer punto de cambio
#' @param tau2 valor del segundo punto de cambio
#' @param nhpp_dist nombre de tasa de NHPP
#' @param theta vector de parámetros de verosimilitud del NHPP
#' @export
D_log_likelihood_region_weibull <- function(vec_d_i, tau1, tau2, theta, nhpp_dist = "W") {
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
