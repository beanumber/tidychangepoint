#' Mean cumulative exceedances function
#' @export
#' @return a numeric vector of length equal to the [exceedances] of `x`
#' @examples
#' tau <- changepoints(lista_AG)
#' theta <- cpt_best_params(lista_AG$segmenter)
#' media_acumulada(x = as.ts(lista_AG), tau, theta)
#' 

media_acumulada <- function(x, tau, theta, dist = "weibull") {
  if (dist == "weibull") {
    d <- function(x, a, b) {
#      -pweibull(x, a, b, lower = FALSE, log = TRUE)
      (x/b)^a
    }
  }
  tau_padded <- pad_tau(x, tau)
  regions <- cut_inclusive(1:length(x), tau_padded)
  theta_calc <- theta |>
    dplyr::mutate(
      region = unique(regions),
      tau_prev = utils::head(tau_padded, -1),
      tau_this = utils::tail(tau_padded, -1),
      m_prev = ifelse(tau_prev == 1, 0, d(tau_prev, alpha, beta)),
      m_this = d(tau_this, alpha, beta),
      cum_m_prev = cumsum(m_prev),
      cum_m_this = cumsum(dplyr::lag(m_this, 1, 0)),
      cum_m_net = cum_m_this - cum_m_prev
    )

  out <- tibble::tibble(
    t = exceedances(x),
    region = cut_inclusive(t, tau_padded)
  ) |>
    dplyr::left_join(theta_calc, by = "region") |>
    dplyr::mutate(
      m_i = d(t, alpha, beta),
      m = m_i + cum_m_net,
#      m_carlos = m_carlos,
#      equal = m_carlos == m
    )
  out$m
}
