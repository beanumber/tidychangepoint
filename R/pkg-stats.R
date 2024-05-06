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

#' @rdname MDL
#' @details
#'  These quantites should be [base::attributes()] of the object returned by 
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
