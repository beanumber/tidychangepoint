#' @rdname fit_nhpp
#' @export
#' @examples
#' fit_nhpp_region_alt(exceedances(lista_AG), 0, 575)
#' fit_nhpp_region(exceedances(lista_AG), 0, 575, initial_val_optim = c(1, 10))
fit_nhpp_region_alt <- function(exc, tau_left, tau_right, 
                                params = parameters_weibull(), ...) {
  # Definimos las funciones que vamos a utilizar para encontrar el mínimo
  my_fn <- function(theta) {
    - (log_likelihood_region_weibull(exc, tau_left, tau_right, theta = theta) + 
         log_prior_region_weibull(theta = theta))
  }
  my_gn <- function(theta) {
    - (D_log_likelihood_region_weibull(exc, tau_left, tau_right, theta = theta) + 
         D_log_prior_region_weibull(theta = theta))
  }
  # Calculamos el mínimo
  (val_optimos <- stats::optim(
    c(params$shape$initial_value, params$scale$initial_value),
    fn = my_fn, 
    gr = my_gn,
    lower = c(params$shape$lower_bound, params$scale$lower_bound), 
    upper = c(params$shape$upper_bound, params$scale$upper_bound),
    method = "L-BFGS-B",
    ... = ...
  ))
  
  val_optimos$logLik <- exc |>
    log_likelihood_region_weibull(tau_left, tau_right, theta = val_optimos$par)
  
  return(val_optimos)
}


#' Fit a non-homogeneous Poisson process model to the exceedances of a time series. 
#' 
#' @description
#' Any times series can be modeled as a non-homogeneous Poisson process of the
#' locations of the exceedances of the mean of the series. This function
#' uses the [BMDL()] criteria to determine the best fit parameters for each 
#' region defined by the changepoint set returned by [changepoints()].
#' @param x A time series
#' @param tau A vector of changepoints. 
#' @return A `tbl_df` with each row representing one region. 
#' 
#' @export
#' @examples
#' fit_nhpp(DataCPSim, tau = 826)
#' fit_nhpp(as.ts(lista_AG), tau = changepoints(lista_AG))

fit_nhpp <- function(x, tau) {
  exc <- exceedances(x)
  exc_by_tau <- exc |>
    split(cut_inclusive(exc, pad_tau(tau, length(x))))

  regions_df <- tibble::tibble(
    region = names(exc_by_tau),
    exceedances = exc_by_tau,
    begin = c(0, tau),
    end = c(tau, length(x))
  )
  
  res <- regions_df |>
    purrr::pmap(
      function(region, exceedances, begin, end) fit_nhpp_region_alt(exceedances, begin, end)
    )
#  fit_nhpp_region(t_by_tau[[1]], endpoints[[1]][1], endpoints[[1]][2])
  
  get_params <- function(z) {
    cbind(
      data.frame(
        "log_posterior" = -z$value,
        "logLik" = z$logLik
      ), 
      data.frame(t(z$par))
    )
  }
  
  out <- res |>
    purrr::map(get_params) |>
    purrr::list_rbind()
  
  # to fix and generalize later
  if ("W" %in% c("W", "MO", "GO")) {
    names_params <- c("alpha", "beta")
  } else {
    names_params <- c("alpha", "beta", "sigma")
  }
  names(out)[3:ncol(out)] <- names_params
  
  out <- dplyr::bind_cols(regions_df, out)
  class(out) <- c("nhpp", class(out))
  
  return(out)
}

#' @rdname fit_nhpp
#' @export
logLik.nhpp <- function(object, ...) {
  ll <- sum(object$logLik)
  attr(ll, "df") <- length(changepoints(object))
  class(ll) <- "logLik"
  return(ll)
}

#' @rdname fit_nhpp
#' @export
BMDL.nhpp <- function(x, ...) {
  tau <- changepoints(x)
  n <- max(x$end)
  penalty_mdl(pad_tau(tau, n)) - sum(x$log_posterior)
}

#' @rdname fit_nhpp
#' @references Zhang and Seigmmund (2007) for MBIC: \doi{10.1111/j.1541-0420.2006.00662.x}
#' @export
MBIC.nhpp <- function(object, ...) {
  tau <- changepoints(object)
  m <- length(tau)
  r <- tau / length(object)
  -(1/2) * (3 * m * log(length(object)) + sum(r)) 
}

#' @rdname fit_nhpp
#' @export
changepoints.nhpp <- function(x, ...) {
  c(x$begin, x$end) |>
    unique() |>
    unpad_tau()
}


#' Mean cumulative exceedances function
#' @param t The exceedances of a time series
#' @param tau A set of changepoints
#' @param theta A data.frame of best-fit parameters
#' @param n the legnth of the original time series
#' @param dist Name of the distribution
#' @export
#' @return a numeric vector of length equal to the [exceedances] of `x`
#' @examples
#' tau <- changepoints(lista_AG)
#' theta <- fit_nhpp(lista_AG$segmenter, tau)
#' cdf_exceedances_est(exceedances(as.ts(lista_AG)), tau, theta, length(lista_AG))
#' 

cdf_exceedances_est <- function(t, tau, theta, n, dist = "weibull") {
  if (dist == "weibull") {
    d <- mweibull
  }
  tau_padded <- pad_tau(tau, n)
  regions <- cut_inclusive(1:n, tau_padded)
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
    t = t,
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
