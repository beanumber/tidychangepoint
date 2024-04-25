globalVariables(c(
  "tau_prev", "tau_this", "m_prev", "m_this", "cum_m_this", "cum_m_prev", "m_i",
  "cum_m_net", "idx", "intensity", "param_alpha", "param_beta"
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
#' @family model-fitting
#' @examples
#' fit_nhpp(DataCPSim, tau = 826)
#' fit_nhpp(DataCPSim, tau = 826, threshold = 20)
#' fit_nhpp(DataCPSim, tau = 826, threshold = 200)
#' fit_nhpp(DataCPSim, tau = changepoints(segment(DataCPSim, method = "pelt")))

fit_nhpp <- function(x, tau, ...) {
  args <- list(...)
  if (is.numeric(args[["threshold"]])) {
    threshold <- args[["threshold"]]
  } else {
    threshold <- mean(x, na.rm = TRUE)
  }
#  message(paste("threshold:", threshold))
  exc <- exceedances(x, threshold = threshold)
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
      data.frame(t(z$par)),
      tibble::tibble(
        "logPost" = -z$value,
        "logLik" = z$logLik
      )
    )
  }
  
  region_params <- res |>
    purrr::map(get_params) |>
    purrr::list_rbind()
  
  # to fix and generalize later
  if ("W" %in% c("W", "MO", "GO")) {
    names_params <- c("param_alpha", "param_beta")
  } else {
    names_params <- c("param_alpha", "param_beta", "param_sigma")
  }
  names(region_params)[1:length(names_params)] <- names_params
  
  region_params <- regions_df |>
    dplyr::select(region) |>
    dplyr::bind_cols(region_params)
  
  regions <- x |>
    as.ts() |>
    split_by_tau(tau)
  mu_seg <- regions |>
    purrr::map_dbl(mean)
  n_seg <- regions |>
    purrr::map_int(length)
  
  out <- mod_cpt(
    x = as.ts(x),
    tau = tau,
    region_params = region_params,
    model_params = c("threshold" = threshold),
    fitted_values = rep(mu_seg, n_seg),
    model_name = "nhpp"
  )
  class(out) <- c("nhpp", class(out))
  return(out)
}

# Register model-fitting functions
fit_nhpp <- fun_cpt("fit_nhpp")

#' @rdname exceedances
#' @param threshold A value above which to exceed. Default is the [mean()]
#' @export
#' @examples
#' fit_nhpp(DataCPSim, tau = 826) |> exceedances()
#' fit_nhpp(DataCPSim, tau = 826, threshold = 200) |> exceedances()
exceedances.nhpp <- function(x, ...) {
  t <- x$model_params[["threshold"]]
  exceedances(as.ts(x), threshold = t, ...)
}

#' @rdname fit_nhpp
#' @param object An `nhpp` object
#' @export
logLik.nhpp <- function(object, ...) {
  ll <- sum(object$region_params[["logLik"]])
  as.logLik(object, ll)
}

#' @rdname BMDL
#' @export
BMDL.nhpp <- function(object, ...) {
  logPrior <- sum(object$region_params[["logPost"]]) - logLik(object) |>
    as.double()
  MDL(object) - 2 * logPrior
}

#' @rdname fit_nhpp
#' @export
glance.nhpp <- function(x, ...) {
  out <- NextMethod()
  out |>
    dplyr::mutate(
      BMDL = BMDL(x)
    )
}

#' @rdname fit_nhpp
#' @param x An `nhpp` object
#' @param dist Name of the distribution
#' @export
#' @return a numeric vector of length equal to the [exceedances] of `x`
#' @examples
#' nhpp <- fit_nhpp(DataCPSim, tau = 826)
#' mcdf(nhpp)
#' nhpp <- fit_nhpp(DataCPSim, tau = 826, threshold = 200)
#' mcdf(nhpp)
mcdf <- function(x, dist = "weibull") {
  if (dist == "weibull") {
    d <- mweibull
  }
  t <- exceedances(x)
  n <- nobs(x)
  tau <- changepoints(x)
  tau_padded <- pad_tau(tau, n)
  
  theta_calc <- x |>
    tidy() |>
    dplyr::mutate(
      m_prev = ifelse(begin == 1, 0, d(begin, param_alpha, param_beta)),
      m_this = d(end, param_alpha, param_beta),
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
      m_i = d(t, param_alpha, param_beta),
      m = m_i + cum_m_net,
      #      m_carlos = m_carlos,
      #      equal = m_carlos == m
    )
  out$m
}

#' @rdname fit_nhpp
#' @export
#' @examples
#' diagnose(fit_nhpp(DataCPSim, tau = 826))
#' diagnose(fit_nhpp(DataCPSim, tau = 826, threshold = 200))

diagnose.nhpp <- function(x, ...) {
  n <- nobs(x)
  
  exc <- exceedances(x)
  z <-exc |>
    tibble::enframe(name = "cum_exceedances", value = "t_exceedance") |>
    dplyr::mutate(
      m = mcdf(x)
    ) |>
    # always add the last observation
    dplyr::bind_rows(
      data.frame(
        cum_exceedances = c(0, length(exc)), 
        t_exceedance = c(0, n),
        m = c(0, length(exc))
      )
    ) |>
    dplyr::mutate(
      lower = stats::qpois(0.05, lambda = m),
      upper = stats::qpois(0.95, lambda = m),
    ) |>
    dplyr::distinct()
  
  ggplot2::ggplot(data = z, ggplot2::aes(x = t_exceedance, y = cum_exceedances)) +
    ggplot2::geom_vline(data = tidy(x), ggplot2::aes(xintercept = end), linetype = 3) +
    ggplot2::geom_abline(intercept = 0, slope = 0.5, linetype = 3) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Time Index (t)", limits = c(0, n)) +
    ggplot2::scale_y_continuous("Cumulative Number of Exceedances (N)") +
    ggplot2::geom_line(ggplot2::aes(y = m), color = "red") +
    ggplot2::geom_line(ggplot2::aes(y = lower), color = "blue") +
    ggplot2::geom_line(ggplot2::aes(y = upper), color = "blue") +
    ggplot2::labs(
      title = "Exceedances of the threshold over time",
      subtitle = paste("Total exceedances:", length(exc), " -- Threshold:", x$model_params[["threshold"]])
    )
}


#' @rdname fit_nhpp
#' @export
#' @examples
#' plot_intensity(fit_nhpp(DataCPSim, tau = 826))
#' mod <- segment(bogota_pm, method = "pelt")
#' plot_intensity(fit_nhpp(bogota_pm, tau = changepoints(mod)))

plot_intensity <- function(x, ...) {
  z <- x |>
    tidy() |>
    dplyr::mutate(
      idx = purrr::map2(begin, end, ~seq(from = .x, to = .y))
    ) |>
    dplyr::select(region, param_alpha, param_beta, idx) |>
    tidyr::unnest(idx) |>
    dplyr::mutate(intensity = iweibull(idx, shape = param_alpha, scale = param_beta))
  
  ggplot2::ggplot(data = z, ggplot2::aes(x = idx, y = intensity)) +
    ggplot2::geom_vline(data = tidy(x), ggplot2::aes(xintercept = end), linetype = 3) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Time Index (t)") +
    ggplot2::scale_y_continuous("Value of Intensity Function") +
    ggplot2::labs(
      title = "Value of Intensity function over Time",
      subtitle = "Weibull"
    )
}
