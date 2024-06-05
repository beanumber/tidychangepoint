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
#' mod <- fit_meanshift_norm_ar1(DataCPSim, tau)
#' logLik(mod)
#' deg_free(mod)
#' 
#' cpts <- c(1700, 1739, 1988)
#' ids <- time2tau(cpts, as_year(time(CET)))
#' mod <- fit_meanshift_norm(CET, tau = ids)
#' glance(mod)
#' mod <- fit_meanshift_norm_ar1(CET, tau = ids)
#' glance(mod)

fit_meanshift <- function(x, tau, distribution = "norm", ...) {
  y <- as.numeric(as.ts(x))
  N <- length(y) # length of the series
  m <- length(tau) # Number of CPTs
  if (!is_valid_tau(tau, N)) {
    stop("Invalid changepoint set")
  } else {
    tau <- unique(tau)
  }
  
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
  } else {
    mu_seg <- y_seg |>
      purrr::map_dbl(mean)
    
    y_hat <- rep(mu_seg, seg_len)
  }
  
  out <- mod_cpt(
    x = y,
    tau = tau,
    region_params = tibble::tibble(
      region = names(y_seg), 
      param_mu = unname(mu_seg)
    ),
    model_params = c(sigma_hatsq = sum((y - y_hat)^2) / N),
    fitted_values = unname(y_hat),
    model_name = paste("meanshift", distribution, sep = "_"),
    distribution = distribution
  )
  return(out)
}

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
  
  out <- mod_cpt(
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
fit_meanshift_norm <- function(x, tau, ...) {
  fit_meanshift(x, tau, distribution = "norm", ...)
}

#' @rdname fit_meanshift
#' @export
fit_meanshift_lnorm <- function(x, tau, ...) {
  out <- fit_meanshift(x, tau, distribution = "lnorm", ...)
  class(out) <- c("mod_cpt_lnorm", class(out))
  return(out)
}

#' @rdname fit_meanshift
#' @inheritParams stats::logLik
#' @export
logLik.meanshift_lnorm <- function(object, ...) {
  extra_term <- sum(log(as.ts(object)))
  out <- NextMethod()
  out - extra_term
}

#' @rdname fit_meanshift
#' @export
fit_meanshift_norm_ar1 <- function(x, tau, ...) {
  fit_meanshift_norm(x, tau,  ...) |>
    autoregress_errors()
}


fit_meanshift_norm <- fun_cpt("fit_meanshift_norm")
fit_meanshift_lnorm <- fun_cpt("fit_meanshift_lnorm")
# fit_meanshift_pois <- fun_cpt("fit_meanshift_pois")
fit_meanshift_norm_ar1 <- fun_cpt("fit_meanshift_norm_ar1")