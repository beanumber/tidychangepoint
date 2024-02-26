globalVariables(c(
  "tau_prev", "tau_this", "m_prev", "m_this", "cum_m_this", "cum_m_prev", "m_i",
  "cum_m_net"
))

#' @rdname fit_nhpp
#' @param exc Output from [exceedances()]
#' @param tau_left left-most changepoint
#' @param tau_right right-most changepoint
#' @param params Output from [parameters_weibull()]
#' @param ... arguments passed to [stats::optim()]
#' @export
#' @examples
#' fit_nhpp_region(exceedances(DataCPSim), 0, 575)
fit_nhpp_region <- function(exc, tau_left, tau_right, 
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
#' fit_nhpp(DataCPSim, tau = changepoints(segment(DataCPSim, method = "pelt")))

fit_nhpp <- function(x, tau) {
  exc <- exceedances(x)
  padded_tau <- pad_tau(tau, length(x))
  exc_by_tau <- exc |>
    split(cut_inclusive(exc, padded_tau))

  regions_df <- tibble::tibble(
    region = names(exc_by_tau),
    exceedances = exc_by_tau,
    begin = utils::head(padded_tau, -1),
    end = utils::tail(padded_tau, -1)
  )
  
  res <- regions_df |>
    purrr::pmap(
      function(region, exceedances, begin, end) fit_nhpp_region(exceedances, begin, end)
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
length.nhpp <- function(x, ...) {
  max(x$end)
}

#' @rdname fit_nhpp
#' @export
nobs.nhpp <- function(object, ...) {
  max(object$end)
}

#' @rdname fit_nhpp
#' @param object An `nhpp` object
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
  penalty_mdl(pad_tau(tau, n), N = length(exceedances(x))) - sum(x$log_posterior)
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
    unpad_tau() |>
    as.integer()
}

#' @rdname fit_nhpp
#' @export
exceedances.nhpp <- function(x, ...) {
  x$exceedances |>
    purrr::list_c()
}


#' @rdname fit_nhpp
#' @param x An `nhpp` object
#' @param dist Name of the distribution
#' @export
#' @return a numeric vector of length equal to the [exceedances] of `x`
#' @examples
#' nhpp <- fit_nhpp(DataCPSim, tau = 826)
#' mcdf(nhpp)
#' 

mcdf <- function(x, dist = "weibull") {
  if (dist == "weibull") {
    d <- mweibull
  }
  t <- exceedances(x)
  n <- length(x)
  tau <- changepoints(x)
  tau_padded <- pad_tau(tau, n)

  theta_calc <- x |>
    # why????
    tibble::as_tibble() |>
    dplyr::mutate(
      m_prev = ifelse(begin == 1, 0, d(begin, alpha, beta)),
      m_this = d(end, alpha, beta),
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

#' @rdname fit_nhpp
#' @export
#' @examples
#' plot(fit_nhpp(DataCPSim, tau = 826))

plot.nhpp <- function(x, ...) {
  n <- length(x)
  
  z <- exceedances(x) |>
    tibble::enframe(name = "cum_exceedances", value = "t_exceedance") |>
    dplyr::mutate(
      m = mcdf(x)
    ) |>
    # always add the last observation
    dplyr::bind_rows(
      data.frame(
        cum_exceedances = c(0, length(exceedances(x))), 
        t_exceedance = c(0, n),
        m = c(0, length(exceedances(x)))
      )
    ) |>
    dplyr::mutate(
      lower = stats::qpois(0.05, lambda = m),
      upper = stats::qpois(0.95, lambda = m),
    ) |>
    dplyr::distinct()
  
  ggplot2::ggplot(data = z, ggplot2::aes(x = t_exceedance, y = cum_exceedances)) +
    ggplot2::geom_vline(data = x, ggplot2::aes(xintercept = end), linetype = 3) +
    ggplot2::geom_abline(intercept = 0, slope = 0.5, linetype = 3) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Time Index (t)", limits = c(0, n)) +
    ggplot2::scale_y_continuous("Cumulative Number of Exceedances (N)") +
    ggplot2::geom_line(ggplot2::aes(y = m), color = "red") +
    ggplot2::geom_line(ggplot2::aes(y = lower), color = "blue") +
    ggplot2::geom_line(ggplot2::aes(y = upper), color = "blue") +
    ggplot2::labs(
      title = "Exceedances of the mean over time",
      subtitle = paste("Total exceedances:", length(exceedances(x)))
    )
}

