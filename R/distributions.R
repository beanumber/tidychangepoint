

#' Evaluanción de rate function (la derivada de la mean)
#'
#' @param t valor a evaluar (real)
#' @param rf_type tipo de rate function (ver articulo A Gibbs Sampling Algorithm
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
#' rf_type <- "EW"
#' rate_fn(t, rf_type, theta)
#' rf_type <- "GGO"
#' rate_fn(t, rf_type, theta)
#' rf_type <- "MO"
#' rate_fn(t, rf_type, theta)
#' rf_type <- "GO"
#' rate_fn(t, rf_type, theta)
#'
rate_fn <- function(t, rf_type, theta) {
  # W -> theta = c(alpha, sigma)
  if (rf_type == "W") {
    return((theta[1] / theta[2]) * (t / theta[2])^(theta[1] - 1))
  }
  # EW -> theta = c(alpha,beta,sigma)
  if (rf_type == "EW") {
    return(theta[1] * theta[2] * (1 - (exp(-t / theta[3]))^theta[1])^(theta[2] - 1) *
             (exp(-t / theta[3]))^theta[1] *
             (t / theta[3])^(theta[1] - 1) /
             (theta[3] * (1 - (1 - exp(-(t / theta[3])^theta[1]))^theta[2])))
  }
  # GGO -> theta = c(alpha,beta,gamma)
  if (rf_type == "GGO") {
    return(theta[1] * theta[2] * theta[3] * t^(theta[3] - 1) * exp(-theta[2] * t^theta[3]))
  }
  # MO -> theta = c(alpha,beta)
  if (rf_type == "MO") {
    return(theta[2] / (t + theta[1]))
  }
  # GO -> theta = c(alpha,beta)
  if (rf_type == "GO") {
    return(theta[1] * theta[2] * exp(-theta[2] * t))
  } else {
    print("no se tiene esa rf_type; rate_fn")
  }
}


#' Evaluanción de mean function (la integral de la rate)
#'
#' @param t valor a evaluar (real)
#' @param rf_type tipo de mean function (ver libro de Eliane pdf 40 eq (3.12));
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
#' rf_type <- "EW"
#' mean_fn(t, rf_type, theta)
#' rf_type <- "GGO"
#' mean_fn(t, rf_type, theta)
#' rf_type <- "MO"
#' mean_fn(t, rf_type, theta)
#' rf_type <- "GO"
#' mean_fn(t, rf_type, theta)
#'
mean_fn <- function(t, rf_type, theta) {
  if (rf_type == "W") {
    return((t / theta[2])^theta[1])
  }
  if (rf_type == "EW") {
    return(-log(1 - (1 - exp(-(t / theta[3])^theta[1]))^theta[2]))
  }
  if (rf_type == "GGO") {
    return(theta[1] * (1 - exp(-theta[2] * t^theta[3])))
  }
  if (rf_type == "MO") {
    return(theta[2] * log(1 + t / theta[1]))
  }
  if (rf_type == "GO") {
    return(theta[1] * (1 - exp(-theta[2] * t)))
  } else {
    print("no se tiene esa rf_type; mean_fn")
  }
}

#' @export
#' @examples
#' f <- cum_mean(lista_AG)
#' f(0, alpha = 1, beta = 2)
#' 
cum_mean <- function(x, ...) {
  if (x$param$rf_type == "W") {
    return(function(x, alpha, beta) (x/beta)^alpha)
  }
  return(NULL)
}

