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
#' ids <- time2tau(cpts, as_year(time(CET)))
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
  out$sigma_hatsq <- sum((out$residuals)^2) / n
  class(out) <- c("lmshift", class(out))
  if (ar1) {
    out <- autoregress_errors(out)
    model_name <- paste0(model_name, "_ar1")
  }
  out$tau <- tau
  out$trends <- trends
  out$ar1 <- ar1
  out$model_name <- model_name
  return(out)
}

autoregress_errors <- function(mod, ...) {
  n <- nobs(mod)
  resid <- mod$residuals
  
  y <- as.ts(mod)
  
  phi_hat <- sum(utils::head(resid, -1) * utils::tail(resid, -1)) / sum(resid^2)
  d <- sum((utils::head(resid, -1) - utils::tail(resid, -1))^2) / sum(resid^2)
  y_hat <- mod$fitted.values + c(0, phi_hat * utils::head(resid, -1))
  sigma_hatsq <- sum((y - y_hat)^2) / n
  
  out <- mod
  out$fitted.values <- y_hat
  out$residuals <- y - y_hat
  out$sigma_hatsq <- sigma_hatsq
  out$phi_hat <- phi_hat
  out$durbin_watson <- d
  out$model_name <- paste0(out$model_name, "_ar1")
  return(out)
}

#' Methods for lmshift objects
#' @name lmshift-generics
#' @param x An `lmshift` object, typically the output from [fit_lmshift()]`
#' @export
#' @examples
#' cpts <- fit_lmshift(DataCPSim, tau = 365)
#' as.ts(cpts)
as.ts.lmshift <- function(x, ...) {
  as.ts(x$model$y)
}


#' @rdname lmshift-generics
#' @inheritParams stats::logLik
#' @export
logLik.lmshift <- function(object, ...) {
  out <- NextMethod()
  m <- length(object$tau)
  attr(out, "real_params_estimated") <- object$rank
  attr(out, "df") <- m + object$rank + 1 + (object$ar1)
  attr(out, "tau") <- object$tau
  attr(out, "ar1") <- object$ar1
  return(out)
}

#' @rdname lmshift-generics
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

#' @rdname changepoints
#' @export
changepoints.lmshift <- function(x, ...) {
  x$tau |>
    as.integer()
}