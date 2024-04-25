#' Fast implementation of meanshift model
#' 
#' @inheritParams fit_lmshift
#' @param distribution A character indicating the distribution of the data. 
#' Should match R distribution function naming conventions 
#' (e.g., "norm" for the Normal distribution, etc.)
#' @export
#' @family model-fitting
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

fit_meanshift <- function(x, tau, distribution = "norm", ...) {
  y <- as.numeric(as.ts(x))
  N <- length(y) # length of the series
  m <- length(tau) # Number of CPTs
  
  y_seg <- y |>
    split_by_tau(tau)
  seg_len <- y_seg |>
    purrr::map_int(length)
  
  # Use the MLE estimates for the means
  if (distribution == c("lnorm")) {
    mu_seg <- y_seg |>
      purrr::map(log) |>
      purrr::map_dbl(mean)
    
    y_hat <- rep(exp(mu_seg), seg_len)
    sigma_hatsq <- sum((log(y) - y_hat)^2) / N
  } else {
    mu_seg <- y_seg |>
      purrr::map_dbl(mean)
    
    y_hat <- rep(mu_seg, seg_len)
    sigma_hatsq <- sum((y - y_hat)^2) / N
  }
  
  out <- mod_default(
    x = y,
    tau = tau,
    region_params = tibble::tibble(
      region = names(y_seg), 
      param_mu = unname(mu_seg)
    ),
    model_params = c(sigma_hatsq = sigma_hatsq),
    fitted_values = unname(y_hat),
    model_name = "meanshift",
    distribution = distribution
  )
  class(out) <- c(paste(out$model_name, distribution, sep = "_"), class(out))
  return(out)
}

attr(fit_meanshift, "model_name") <- "meanshift"

#' @rdname fit_meanshift
#' @export
fit_meanshift2 <- function(x, tau, ...) {
  y <- as.numeric(as.ts(x))
  N <- length(y) # length of the series
  m <- length(tau) # Number of CPTs
  
  regions <- y |>
    split_by_tau(tau) |>
    purrr::map(fit_meanshift_region, ...)
  
  region_names <- names(regions)
  
  region_params <- regions |>
    purrr::map(1) |>
    purrr::list_rbind() |>
    dplyr::mutate(region = region_names, .before = dplyr::everything())
  
  y_hat <- regions |>
    purrr::map(2) |>
    purrr::list_c()
  
  out <- mod_default(
    x = y,
    tau = tau,
    region_params = region_params,
    model_params = c(
      sigma_hatsq = sum((y - y_hat)^2) / N
    ),
    fitted_values = y_hat,
    model_name = "meanshift"
  )
  return(out)
}
attr(fit_meanshift2, "model_name") <- "meanshift"


fit_meanshift_region <- function(x, ...) {
  y <- as.numeric(x)
  N <- length(y)
  mu <- mean(y, na.rm = TRUE)
  list(
    region_params = tibble::tibble(
      param_mu = mu
    ),
    fitted_values = rep(mu, N)
  )
}


#' @rdname fit_meanshift
#' @export
fit_meanshift_ar1 <- function(x, tau, ...) {
  fit_meanshift(x, tau,  ...) |>
    autoregress_errors()
}

attr(fit_meanshift_ar1, "model_name") <- "meanshift_ar1"
