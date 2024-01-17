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

#' @export
#' @examples
#' tau <- cpt_best(lista_AG)
#' theta <- cpt_best_params(lista_AG)
#' media_acumulada(x = lista_AG$data, tau, theta)
#' 

media_acumulada <- function(x, tau, theta, dist = "weibull") {
  if (dist == "weibull") {
    d <- function(x, a, b) {
#      -pweibull(x, a, b, lower = FALSE, log = TRUE)
      (x/b)^a
    }
  }
  if (tau[1] != 1) {
    tau <- c(1, tau)
  }
  if (tau[length(tau)] != length(x)) {
    tau <- c(tau, length(x))
  }
  regions <- cut(1:length(x), breaks = tau, include.lowest = TRUE)
  theta <- theta |>
    dplyr::mutate(
      region = unique(regions),
      tau_prev = utils::head(tau, -1),
      tau_this = utils::tail(tau, -1),
      m_prev = d(tau_prev, alpha, beta),
      m_this = d(tau_this, alpha, beta),
      cum_m_prev = cumsum(m_prev),
      cum_m_this = cumsum(m_this)
    )

  tibble::tibble(
    t = exceedances(x),
    region = cut(t, breaks = tau, include.lowest = TRUE)
  ) |>
    dplyr::left_join(theta, by = "region") |>
    dplyr::mutate(
      m_i = d(t, alpha, beta),
      m = m_i + cum_m_prev + cum_m_this
    )
}

