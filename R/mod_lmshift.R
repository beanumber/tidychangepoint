globalVariables(c("adj.r.squared", "df", "df.residual", "p.value", "statistic"))

#' Regression-based model fitting
#' @param x A time series
#' @param tau a set of indices representing a changepoint set
#' @param trends logical indicating whether you want trends within regions
#' @param ar1 logicial indicating whether you want auto-regressive lag 1 errors
#' @param ... arguments passed to [stats::lm()]
#' @export
#' @examples
#' tau <- c(365, 826)
#' mod <- fit_lmshift(DataCPSim, tau)
#' logLik(mod)
#' deg_free(mod)
#' 
#' cpts <- c(1700, 1739, 1988)
#' ids <- time2tau(cpts, lubridate::year(time(CET)))
#' mod <- fit_lmshift(CET, tau = ids)
#' glance(mod)
#' glance(fit_lmshift(CET, tau = ids, trends = TRUE))
#' glance(fit_lmshift(CET, tau = ids, ar1 = TRUE))
#' glance(fit_lmshift(CET, tau = ids, trends = TRUE, ar1 = TRUE))

fit_lmshift <- function(x, tau, trends = FALSE, ar1 = FALSE, ...) {
  n <- length(x)
  ds <- data.frame(y = as.ts(x), t = 1:n)
  if (1 %in% tau) {
    tau <- utils::tail(tau, -1)
  }
  if (length(tau) < 1) {
    form <- "y ~ 1"
    model_name <- "null"
  } else {
    if (trends) {
      terms <- paste(paste("t * (t >=", tau, ")"), collapse = " + ")
      model_name <- "trendshift"
    } else {
      terms <- paste(paste("(t >=", tau, ")"), collapse = " + ")
      model_name <- "meanshift"
    }
    form <- paste("y ~", terms)
  }
  
  out <- stats::lm(stats::as.formula(form), data = ds, ...)
  if (ar1) {
    out <- autoregress_errors(out)
    model_name <- paste0(model_name, "_ar1")
  }
  out$tau <- tau
  out$trends <- trends
  out$ar1 <- ar1
  out$model_name <- model_name
  class(out) <- c("lmshift", class(out))
  return(out)
}

autoregress_errors <- function(mod, ...) {
  n <- nobs(mod)
  resid <- mod$residuals
  
  phi_hat <- sum(utils::head(resid, -1) * utils::tail(resid, -1)) / sum(resid^2)
  y_hat <- mod$fitted.values + c(0, phi_hat * utils::head(resid, -1))
  sigma_hat_sq <- sum((mod$model$y - y_hat)^2) / n
  
  out <- mod
  out$fitted.values <- y_hat
  out$residuals <- mod$model$y - y_hat
  return(out)
}

#' @rdname fit_meanshift
#' @inheritParams stats::logLik
#' @export
logLik.lmshift <- function(object, ...) {
  out <- NextMethod()
  m <- length(object$tau)
  params_estimated <- object$rank
  attr(out, "df") <- m + params_estimated + 1 + (object$ar1)
  return(out)
}

#' @rdname fit_meanshift
#' @export
MBIC.lmshift <- function(object, ...) {
  MBIC(logLik(object))
}

#' @rdname fit_meanshift
#' @export
MDL.lmshift <- function(object, ...) {
  MDL(logLik(object))
}

#' @rdname fit_meanshift
#' @export
glance.lmshift <- function(x, ...) {
  tibble::tibble(
    pkg = "tidychangepoint",
    version = package_version(utils::packageVersion("tidychangepoint")),
    algorithm = x$model_name,
    params = list(ar1 = x$ar1),
    num_cpts = length(changepoints(x)),
    rmse = sqrt(mean(x$residuals^2)),
    logLik = as.double(logLik(x)),
    AIC = AIC(x),
    BIC = BIC(x),
    MBIC = MBIC(x),
    MDL = MDL(x)
  )
}

#' @rdname fit_meanshift
#' @export
changepoints.lmshift <- function(x, ...) {
  x$tau |>
    as.integer()
}