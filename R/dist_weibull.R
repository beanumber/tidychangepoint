#' Weibull distribution functions
#' 
#' @param x A numeric vector
#' @param shape Shape parameter for Weibull distribution. See [stats::dweibull()].
#' @param scale Scale parameter for Weibull distribution. See [stats::dweibull()].
#' @return A numeric vector
#' @details
#' Intensity function for the Weibull distribution. 
#' \deqn{
#'    iweibull(x) = \left( \frac{shape}{scale} \right) \cdot 
#'                  \left( \frac{x}{scale} \right)^{shape - 1}
#'  }
#' 
#' @export
#' @seealso [stats::dweibull()]
#' @examples
#' # Compute the intensities and plot them
#' iweibull(1, shape = 1, scale = 1)
#' plot(x = 1:10, y = iweibull(1:10, shape = 2, scale = 2))
#' 
iweibull <- function(x, shape, scale = 1) {
  (shape / scale) * (x / scale)^(shape - 1)
}

#' @rdname iweibull
#' @export
#' @details
#' Mean intensity function for the Weibull distribution. 
#' \deqn{
#'    mweibull(x) = \left( \frac{x}{scale} \right)^{shape}
#'  }
#' @examples
#' # Compute various values of the distribution
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
#' @details
#' [parameters_weibull()] returns a `list()` with two components: `shape` 
#' and `scale`, each of which is a `list()` of distribution parameters. 
#' These parameters are used to define the prior distributions for the 
#' hyperparameters.
#' 
#' @export
#' @seealso [stats::dgamma()]
#' @examples
#' # Generate prior distribution hyperparameters
#' parameters_weibull()
#' 

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

#' Log-Likelihood functions for regions (Weibull)
#' @param t vector of [exceedances()]
#' @param tau_left Left endpoint of the region
#' @param tau_right Right endpoint of the region
#' @param theta numeric vector of parameters for the NHPP model
#' @keywords internal
#' @returns A numeric vector
#' 
log_likelihood_region_weibull <- function(t, tau_left, tau_right, theta) {
  (tau_left^theta[1] - tau_right^theta[1]) / theta[2]^theta[1] +
    length(t) * (log(theta[1]) - theta[1] * log(theta[2])) +
    (theta[1] - 1) * sum(log(t))
}

#' @rdname log_likelihood_region_weibull
#' @param params Possibly modified output from [parameters_weibull()]
#' @keywords internal
#' 
log_prior_region_weibull <- function(theta, params = parameters_weibull()) {
  # a.k.a. the shape parameter for the Weibull distribution
  alpha <- params[["shape"]]
  # a.k.a. the scale parameter for the Weibull distribution
  beta <- params[["scale"]]

  (alpha$rate - 1) * log(theta[1]) - alpha$shape * theta[1] + # Exp 74 pag 21
    (beta$rate - 1) * log(theta[2]) - beta$shape * theta[2] # Exp 75 pag 21
}

#' @rdname log_likelihood_region_weibull
#' @keywords internal
#' 
D_log_prior_region_weibull <- function(theta, params = parameters_weibull()) {
  alpha <- params[["shape"]]
  beta <- params[["scale"]]
  # Parcial con respecto a alfa
  p1 <- (-1 - theta[1] * alpha$shape + alpha$rate) / theta[1]
  # Parcial con respecto a beta
  p2 <- (-1 - theta[2] * beta$shape + beta$rate) / theta[2]
  return(c(p1, p2))
}

#' @rdname log_likelihood_region_weibull
#' @keywords internal
#' 
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
