#' Fast implementation of meanshift model
#' 
#' @inheritParams fit_lmshift
#' @param distribution A character indicating the distribution of the data. 
#' Should match R distribution function naming conventions 
#' (e.g., "norm" for the Normal distribution, etc.)
#' @export
#' @details
#' [fit_meanshift_norm()] returns the same model as [fit_lmshift()] with the 
#' `deg_poly` argument set to 0. 
#' However, it is faster on large changepoint sets. 
#' 
#' [fit_meanshift_lnorm()] fit the meanshift model with the assumption of 
#' log-normally distributed data. 
#' 
#' [fit_meanshift_norm_ar1()] applies autoregressive errors. 
#' 
#' @returns A [mod_cpt] object.
#' @family model-fitting
#' @author Xueheng Shi, Ben Baumer
#' @examples
#' # Manually specify a changepoint set
#' tau <- c(365, 826)
#' 
#' # Fit the model
#' mod <- fit_meanshift_norm_ar1(DataCPSim, tau)
#' 
#' # View model parameters
#' logLik(mod)
#' deg_free(mod)
#' 
#' # Manually specify a changepoint set
#' cpts <- c(1700, 1739, 1988)
#' ids <- time2tau(cpts, as_year(time(CET)))
#' 
#' # Fit the model
#' mod <- fit_meanshift_norm(CET, tau = ids)
#' 
#' # Review model parameters
#' glance(mod)
#' 
#' # Fit an autoregressive model
#' mod <- fit_meanshift_norm_ar1(CET, tau = ids)
#' 
#' # Review model parameters
#' glance(mod)
#' 
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

#' @rdname reexports
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