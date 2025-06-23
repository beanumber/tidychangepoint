#' @rdname exceedances
#' @export
exceedances.ts <- function(x, ...) {
  exceedances(as.double(x), ...)
}

#' @rdname exceedances
#' @param threshold A value above which to exceed. Default is the [mean()]
#' @export
exceedances.double <- function(x, threshold = mean(x, na.rm = TRUE), ...) {
  which(x > threshold, ...)
}

#' @rdname HQC
#' @inheritParams stats::logLik
#' @details
#' Computes the Hannan-Quinn information criterion for a model \eqn{M}
#' \deqn{
#'   HQC(\tau, M(y|\hat{\theta}_{\tau})) = 2k \cdot \ln{\ln{n}} - 2 \cdot L_M(y|\hat{\theta}_\tau) \,,
#' }
#' where \eqn{k} is the number of parameters and \eqn{n} is the number of observations.
#' @export
#' @examples
#' HQC(fit_meanshift_norm_ar1(CET, tau = c(42, 330)))
#' HQC(fit_trendshift(CET, tau = c(42, 81, 330)))
HQC.logLik <- function(object, ...) {
  -2 * as.numeric(object) + attr(object, "df") * log(log(nobs(object))) |>
    as.double()
}

#' @rdname MDL
#' @details
#'  These quantities should be [base::attributes()] of the object returned by 
#'  [logLik()].
#' @inheritParams stats::logLik
#' @export
#' @examples
#' MDL(fit_meanshift_norm_ar1(CET, tau = c(42, 330)))
#' MDL(fit_trendshift(CET, tau = c(42, 81, 330)))
MDL.logLik <- function(object, ...) {
  tau <- attr(object, "tau")
  N <- nobs(object)
  m <- length(tau)

  if (m == 0) {
    penalty <- 0
  } else {
    padded_tau <- pad_tau(tau, n = N)
    
    # actually twice the penalty!
    penalty <- attr(object, "num_params_per_region") * sum(log(diff(padded_tau))) + 
      2 * log(m) + 
      2 * sum(log(utils::tail(tau, -1))) +
      (2 + attr(object, "num_model_params")) * log(N)
  }
  
  penalty - 2 * object |>
    as.double()
}

#' @rdname MBIC
#' @references Zhang and Seigmmund (2007) for MBIC: \doi{10.1111/j.1541-0420.2006.00662.x}
#' @export
MBIC.logLik <- function(object, ...) {
  tau <- attr(object, "tau")
  m <- length(tau)
  if (m == 0) {
    penalty <- 0
  } else {
    n <- nobs(object)
    padded_tau <- pad_tau(tau, n)
    penalty <- 3 * m * log(n) + sum(log(diff(padded_tau) / n)) 
  }
  penalty - 2 * object |>
    as.double()
}

#' Fit an ARIMA model
#' @inheritParams fit_meanshift
#' @param ... currently ignored
#' @export
#' @details
#' Fits an ARIMA model using [stats::arima()].
#' 
#' @returns A [mod_cpt] object.
#' @family model-fitting
#' @seealso [changepointGA::ARIMA.BIC()]
#' @examples
#' # Fit a mean-variance model
#' fit_arima(CET, tau = c(42, 330))
#' 
fit_arima <- function(x, tau, ...) { 
  if (!is_valid_tau(tau, length(x))) {
    stop("Invalid changepoint set")
  } else {
    tau <- unique(tau)
  }
  regions <- x |> 
    as.ts() |>
    split_by_tau(tau)
  
  region_mods <- regions |>
    purrr::map(~stats::arima(.x))
  
  fitted_values <- region_mods |>
    purrr::map(~residuals(.x) + coef(.x)) |>
    purrr::list_c() |>
    as.double()
  
  pluck_params <- function(ar) {
    data.frame(
      "param_intercept" = as.double(ar$coef),
      "param_sigma_hatsq" = as.double(ar$sigma2)
    )
  }
  
  region_params <- region_mods |>
    purrr::map(pluck_params) |>
    purrr::list_rbind() |>
    dplyr::mutate(region = names(regions))
  
  mod_cpt(
    x <- as.ts(x),
    tau = tau,
    region_params = region_params,
    model_params = c(),
    fitted_values = fitted_values,
    model_name = "arima"
  )
}

# Register model-fitting functions
fit_arima <- fun_cpt("fit_arima")
