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
#' glance(fit_lmshift_ar1(CET, tau = ids))
#' glance(fit_lmshift_ar1(CET, tau = ids, trends = TRUE))

fit_lmshift <- function(x, tau, trends = FALSE, ...) {
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
    form <- paste("y ~ ", terms)
  }
  
  mod <- stats::lm(stats::as.formula(form), data = ds, ...)

  cptmod(
    x = as.ts(x),
    tau = tau,
    region_params = tbl_coef(mod),
    model_params = c(
      sigma_hatsq = model_variance(mod)
    ),
    fitted_values = fitted(mod),
    model_name = model_name
  )
}

#' @rdname fit_lmshift
#' @export
fit_lmshift_ar1 <- function(x, tau, ...) {
  fit_lmshift(x, tau,  ...) |>
    autoregress_errors()
}
