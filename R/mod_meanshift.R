#' Fast implementation of meanshift model
#' 
#' @inheritParams fit_lmshift
#' @export
#' @author Xueheng Shi
#' @examples
#' tau <- c(365, 826)
#' mod <- fit_meanshift_ar1(DataCPSim, tau)
#' logLik(mod)
#' deg_free(mod)
#' 
#' cpts <- c(1700, 1739, 1988)
#' ids <- time2tau(cpts, as_year(time(CET)))
#' mod <- fit_meanshift(CET, tau = ids)
#' glance(mod)
#' mod <- fit_meanshift_ar1(CET, tau = ids)
#' glance(mod)

fit_meanshift <- function(x, tau, ...) {
  y <- as.numeric(as.ts(x))
  N <- length(y) # length of the series
  m <- length(tau) # Number of CPTs
  
  y_seg <- y |>
    split_by_tau(tau)
  mu_seg <- y_seg |>
    purrr::map_dbl(mean)
  seg_len <- y_seg |>
    purrr::map_int(length)
  y_hat <- rep(mu_seg, seg_len)

  sigma_hatsq <- sum((y - y_hat)^2) / N
  
  out <- cptmod(
    x = y,
    tau = tau,
    region_params = tibble::tibble(mu = mu_seg),
    model_params = c(
      sigma_hatsq = sigma_hatsq
    ),
    fitted_values = y_hat,
    model_name = "meanshift"
  )
  return(out)
}

#' @rdname fit_meanshift
#' @export
fit_meanshift_ar1 <- function(x, tau, ...) {
  fit_meanshift(x, tau,  ...) |>
    autoregress_errors()
}

