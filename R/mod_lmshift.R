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

  regions <- split_by_tau(as.ts(x), tau = tau) |>
    names()
  
  region_params <- mod |>
    tbl_coef() |>
    dplyr::mutate(region = regions)
  
  mod_default(
    x = as.ts(x),
    tau = tau,
    region_params = region_params,
    model_params = c(
      sigma_hatsq = model_variance(mod)
    ),
    fitted_values = fitted(mod),
    model_name = model_name
  )
}

attr(fit_lmshift, "model_name") <- "lmshift"

#' @rdname fit_lmshift
#' @export
fit_lmshift_ar1 <- function(x, tau, ...) {
  fit_lmshift(x, tau,  ...) |>
    autoregress_errors()
}

attr(fit_lmshift_ar1, "model_name") <- "lmshift_ar1"


#' @rdname fit_lmshift
#' @export
#' @examples
#' ds <- data.frame(y = as.ts(CET), t = 1:length(CET))
#' tbl_coef(lm(y ~ 1, data = ds))
#' tbl_coef(lm(y ~ (t >= 42) + (t >= 81), data = ds))
#' tbl_coef(lm(y ~ t * (t >= 42) + t * (t >= 81), data = ds))

tbl_coef <- function(mod, ...) {
  out <- mod |>
    stats::coef() |>
    tibble::enframe(name = "variable", value = "value") |>
    dplyr::mutate(
      region = stringr::str_extract(variable, pattern = "t >= [0-9]+"),
      is_slope = grepl(pattern = "^t$|t:t", variable)
    ) |>
    dplyr::select(-variable) |>
    tidyr::pivot_wider(names_from = "is_slope", values_from = "value") |>
    dplyr::mutate(
      tau = stringr::str_extract(region, pattern = "[0-9]+$") |>
        as.integer(),
      tau = ifelse(is.na(tau), 0, tau),
      param_mu = cumsum(`FALSE`)
    )
  if ("TRUE" %in% names(out)) {
    out <- out |>
      dplyr::mutate(
        param_beta = cumsum(`TRUE`),
        param_mu + tau * param_beta
      )
  }
  vars <- c("region", "param_mu", "param_beta")
  out |>
    dplyr::select(any_of(vars))
}
