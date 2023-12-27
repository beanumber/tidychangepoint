#' funcion media
#' @export
#' @examples
#' funcion_media_acumulada(
#'   i = 1, d = sample(1:1096), alpha = runif(6), 
#'   sigma = runif(6), tau = sample(1:1096, 5)
#'  )
#' 
#'
funcion_media_acumulada <- function(i = 1, d, alpha, sigma, tau) {
  step(-(d[i] - tau[1] - 0.5)) * pow(d[i] / sigma[1], alpha[1]) +
    +step(d[i] - tau[1] - 0.5) * step(-(d[i] - tau[2] - 0.5)) * (pow(d[i] / sigma[2], alpha[2]) + pow(tau[1] / sigma[1], alpha[1]) - pow(tau[1] / sigma[2], alpha[2])) +
    +step(d[i] - tau[2] - 0.5) * step(-(d[i] - tau[3] - 0.5)) * (pow(d[i] / sigma[3], alpha[3]) + pow(tau[2] / sigma[2], alpha[2]) + pow(tau[1] / sigma[1], alpha[1]) - pow(tau[1] / sigma[2], alpha[2]) - pow(tau[2] / sigma[3], alpha[3])) +
    +step(d[i] - tau[3] - 0.5) * (pow(d[i] / sigma[4], alpha[4]) + pow(tau[3] / sigma[3], alpha[3]) + pow(tau[2] / sigma[2], alpha[2]) + pow(tau[1] / sigma[1], alpha[1]) - pow(tau[1] / sigma[2], alpha[2]) - pow(tau[2] / sigma[3], alpha[3]) - pow(tau[3] / sigma[4], alpha[4])) +
    0
}

#' @examples
#' y <- sims$ts1
#' tau <- c(1, 825, 1096)
#' theta <- data.frame(
#'   alpha = c(0.4, 1),
#'   beta = c(1, 2)
#' )
#' media_acumulada(tx = 1:length(y), tau, theta)
#' 

media_acumulada <- function(tx, tau, theta, dist = "weibull") {
  if (dist == "weibull") {
    d <- function(x, a, b) {
#      -pweibull(x, a, b, lower = FALSE, log = TRUE)
      (x/b)^a
    }
  }
  regions <- cut(1:length(tx), breaks = tau, include.lowest = TRUE)
  theta <- theta |>
    mutate(
      region = unique(regions)
    )
  tibble(
    t = tx,
    region = regions
  ) |>
    left_join(theta, by = "region") |>
    mutate(
      m_i = d(t, alpha, beta),
      
    )
}

