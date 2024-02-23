#' Weibull distribution functions
#' 
#' @inheritParams stats::dweibull
#' @param x A numeric vector
#' @param shape Shape parameter for Weibull distribution. See [stats::dweibull()].
#' @param scale Scale parameter for Weibull distribution. See [stats::dweibull()].
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
#' @seealso [stats::dweibull()]
#' @examples
#' iweibull(1, shape = 1, scale = 1)
#' plot(x = 1:10, y = iweibull(1:10, shape = 2, scale = 2))

iweibull <- function(x, shape, scale = 1) {
  (shape / scale) * (x / scale)^(shape - 1)
}


#' @rdname iweibull
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

#' @rdname iweibull
#' @param ... currently ignored
#' @export

parameters_weibull <- function(...) {
  list(
    shape = list(
      dist = "gamma",
      shape = 1,
      rate = 2,
      initial_value = 0.1,
      lower_bound = 0.0001,
      upper_bound = 10
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

#' @rdname iweibull
#' @export
#' @examples
#' log_likelihood_region_weibull(DataCPSim, 0, 575, theta = c(0.5, 2))
#' 
log_likelihood_region_weibull <- function(t, tau_left, tau_right, theta) {
  (tau_left^theta[1] - tau_right^theta[1]) / theta[2]^theta[1] +
    length(t) * (log(theta[1]) - theta[1] * log(theta[2])) +
    (theta[1] - 1) * sum(log(t))
}

#' @rdname iweibull
#' @param params Possibly modified output from [parameters_weibull()]
#' @export
#' @examples
#' log_prior_region_weibull(theta = c(0.5, 2))
log_prior_region_weibull <- function(theta, params = parameters_weibull()) {
  # a.k.a. the shape parameter for the Weibull distribution
  alpha <- params[["shape"]]
  # a.k.a. the scale parameter for the Weibull distribution
  beta <- params[["scale"]]

  (alpha$rate - 1) * log(theta[1]) - alpha$shape * theta[1] + # Exp 74 pag 21
    (beta$rate - 1) * log(theta[2]) - beta$shape * theta[2] # Exp 75 pag 21
}

#' @rdname iweibull
#' @export
#' @examples
#' D_log_prior_region_weibull(theta = c(0.5, 2))
D_log_prior_region_weibull <- function(theta, params = parameters_weibull()) {
  alpha <- params[["shape"]]
  beta <- params[["scale"]]
  # Parcial con respecto a alfa
  p1 <- (-1 - theta[1] * alpha$shape + alpha$rate) / theta[1]
  # Parcial con respecto a beta
  p2 <- (-1 - theta[2] * beta$shape + beta$rate) / theta[2]
  return(c(p1, p2))
}

#' @rdname iweibull
#' @param t vector of exceedances
#' @param tau_left valor del primer punto de cambio
#' @param tau_right valor del segundo punto de cambio
#' @param theta vector de parámetros de verosimilitud del NHPP
#' @export
#' @examples
#' 
#' D_log_likelihood_region_weibull(DataCPSim, 0, 575, theta = c(0.5, 2))
D_log_likelihood_region_weibull <- function(t, tau_left, tau_right, theta) {
  difN <- length(t)
  alpha <- theta[1]
  beta <- theta[2]
  sumlogd <- sum(log(t))
  if (tau_left == 0) { # este es el caso del primer regimen
    # Parcial de alpha
    p1 <- difN / alpha + sumlogd - difN * log(beta) + beta^(-alpha) * tau_right^alpha * (log(beta) - log(tau_right))
    # Parcial de beta
    p2 <- alpha * beta^(-1 - alpha) * (-difN * beta^alpha + tau_right^alpha)
  } else { # para los otros regímenes (o bloques)
    # Parcial de alpha
    p1 <- difN / alpha + sumlogd + beta^(-alpha) * (-(difN * beta^alpha + tau_left^alpha - tau_right^alpha) * log(beta) + tau_left^alpha * log(tau_left) - tau_right^alpha * log(tau_right))
    # Parcial de beta
    p2 <- -alpha * beta^(-1 - alpha) * (difN * beta^alpha + tau_left^alpha - tau_right^alpha)
  }
  return(c(p1, p2))
}
