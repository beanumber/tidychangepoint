#' Regression-based model fitting
#' @export
#' @examples
#' tau <- c(365, 826)
#' mod <- fit_meanshift(DataCPSim, tau)
#' logLik(mod)
#' deg_free(mod)
#' 
#' cpts <- c(1700, 1739, 1988)
#' ids <- time2tau(cpts, lubridate::year(time(CET)))
#' mod <- fit_meanshift(CET, tau = ids)
#' glance(mod)
#' 
#' mod2 <- fit_meanshift_ar1(CET, tau = ids)
#' glance(mod2)
#' 
#' mod <- fit_trendshift(CET, tau = ids, trends = TRUE)
#' logLik(mod)
#' deg_free(mod)

fit_meanshift <- function(x, tau, ...) {
  n <- length(x)
  ds <- data.frame(y = as.ts(x), t = 1:n)
  if (length(tau) < 1) {
    form <- "y ~ 1"
  } 
  form <- paste(
    "y ~",
    paste(
      paste("(t >=", tau, ")"),
      collapse = " + "
    )
  ) |>
    stats::as.formula()
  
  out <- stats::lm(form, data = ds)
  out$tau <- tau
  class(out) <- c("cptshift", class(out))
  return(out)
}

#' @rdname fit_meanshift
#' @export

fit_meanshift_ar1 <- function(x, tau, ...) {
  mod <- fit_meanshift(x, tau, ...)
  n <- nobs(mod)
  resid <- mod$residuals
  
  phi_hat <- sum(head(resid, -1) * tail(resid, -1)) / sum(resid^2)
  y_hat <- mod$fitted.values + c(0, phi_hat * head(resid, -1))
  sigma_hat_sq <- sum((mod$model$y - y_hat)^2) / n
  
  out <- mod
  out$fitted.values <- y_hat
  out$residuals <- mod$model$y - y_hat
  out$rank <- 2 * length(tau) + 3
  out$df.residual <- n - out$rank
  return(out)
}


#' @rdname fit_meanshift
#' @export

fit_trendshift <- function(x, tau, trends = TRUE, ...) {
  n <- length(x)
  ds <- data.frame(y = as.ts(x), t = 1:n)
  if (length(tau) < 1) {
    form <- "y ~ 1"
  } else {
    if (trends) {
      form <- paste(
        "y ~",
        paste(
          paste("t*(t >", tau, ")"),
          collapse = " + "
        )
      ) 
    } else {
      form <- paste(
        "y ~",
        paste(
          paste("(t >", tau, ")"),
          collapse = " + "
        )
      )
    }
  }
  form <- form |>
    stats::as.formula()
  
  stats::lm(form, data = ds)
}

#' @rdname fit_meanshift
#' @export
logLik.cptshift <- function(object, ...) {
  out <- NextMethod()
  attr(out, "df") <- 2 * length(object$tau) + 3
  return(out)
}
