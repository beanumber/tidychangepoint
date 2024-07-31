globalVariables(c("adj.r.squared", "df", "df.residual", "p.value", "statistic", 
                  "variable", "param_mu", "param_beta", "poly_k", "k", "param_beta0"))

#' Regression-based model fitting
#' @param x A time series
#' @param tau a set of indices representing a changepoint set
#' @param deg_poly integer indicating the degree of the polynomial spline to be
#' fit. Passed to [stats::poly()].
#' @param ... arguments passed to [stats::lm()]
#' @details
#' These model-fitting functions use [stats::lm()] to fit the corresponding 
#' regression model to a time series, using the changepoints specified by the 
#'  `tau` argument. 
#' Each changepoint is treated as a categorical fixed-effect, while the `deg_poly`
#' argument controls the degree of the polynomial that interacts with those
#' fixed-effects. 
#' For example, setting `deg_poly` equal to 0 will return the same model as 
#' calling [fit_meanshift_norm()], but the latter is faster for larger changepoint
#' sets because it doesn't have to fit all of the regression models. 
#' 
#' Setting `deg_poly` equal to 1 fits the `trendshift` model. 
#' 
#' 
#' @export
#' @returns A [mod_cpt] object
#' @family model-fitting
#' @examples
#' # Manually specify a changepoint set
#' tau <- c(365, 826)
#' 
#' # Fit the model
#' mod <- fit_lmshift(DataCPSim, tau)
#' 
#' # Retrieve model parameters
#' logLik(mod)
#' deg_free(mod)
#' 
#' # Manually specify a changepoint set
#' cpts <- c(1700, 1739, 1988)
#' ids <- time2tau(cpts, as_year(time(CET)))
#' 
#' # Fit the model
#' mod <- fit_lmshift(CET, tau = ids)
#' 
#' # View model parameters
#' glance(mod)
#' glance(fit_lmshift(CET, tau = ids, deg_poly = 1))
#' glance(fit_lmshift_ar1(CET, tau = ids))
#' glance(fit_lmshift_ar1(CET, tau = ids, deg_poly = 1))
#' glance(fit_lmshift_ar1(CET, tau = ids, deg_poly = 2))
#' 
#' # Empty changepoint sets are allowed
#' fit_lmshift(CET, tau = NULL)
#' 
#' # Duplicate changepoints are removed
#' fit_lmshift(CET, tau = c(42, 42))
#' 
fit_lmshift <- function(x, tau, deg_poly = 0, ...) {
  n <- length(x)
  ds <- data.frame(y = as.ts(x), t = 1:n)
  if (!is_valid_tau(tau, n)) {
    stop("Invalid changepoint set")
  } else {
    tau <- unique(tau)
  }
  if (length(tau) < 1) {
    form <- "y ~ 1"
    model_name <- "null"
  } else {
    if (deg_poly > 0) {
      terms <- paste(paste("poly(t,", deg_poly, ", raw = TRUE) * (t >=", tau, ")"), collapse = " + ")
      if (deg_poly == 1) {
        model_name <- "trendshift"
      } else {
        model_name <- "splineshift"
      }
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
  
  mod_cpt(
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

#' @rdname fit_lmshift
#' @details
#' - [fit_lmshift_ar1()]: will apply auto-regressive lag 1 errors
#' @export
fit_lmshift_ar1 <- function(x, tau, ...) {
  fit_lmshift(x, tau,  ...) |>
    autoregress_errors()
}

#' @rdname fit_lmshift
#' @details
#' - [fit_trendshift()]: will fit a line in each region
#' @export
fit_trendshift <- function(x, tau, ...) {
  fit_lmshift(x, tau, deg_poly = 1, ...)
}

#' @rdname fit_lmshift
#' @details
#' - [fit_trendshift_ar1()]: will fit a line in each region and autoregress lag 1 errors
#' @export
fit_trendshift_ar1 <- function(x, tau, ...) {
  fit_trendshift(x, tau, ...) |>
    autoregress_errors()
}


# Register model-fitting functions
fit_trendshift <- fun_cpt("fit_trendshift")
fit_trendshift_ar1 <- fun_cpt("fit_trendshift_ar1")
fit_lmshift <- fun_cpt("fit_lmshift")
fit_lmshift_ar1 <- fun_cpt("fit_lmshift_ar1")

#' Format the coefficients from a linear model as a tibble
#' @param mod An `lm` model object
#' @param ... currently ignored
#' @returns A [tibble::tbl_df] object containing the fitted coefficients.
#' @export
#' @examples
#' # Convert a time series into a data frame with indices
#' ds <- data.frame(y = as.ts(CET), t = 1:length(CET))
#' 
#' # Retrieve the coefficients from a null model
#' tbl_coef(lm(y ~ 1, data = ds))
#' 
#' # Retrieve the coefficients from a two changepoint model
#' tbl_coef(lm(y ~ (t >= 42) + (t >= 81), data = ds))
#' 
#' # Retrieve the coefficients from a trendshift model
#' tbl_coef(lm(y ~ poly(t, 1, raw = TRUE) * (t >= 42) + poly(t, 1, raw = TRUE) * (t >= 81), data = ds))
#' 
#' # Retrieve the coefficients from a quadratic model
#' tbl_coef(lm(y ~ poly(t, 2, raw = TRUE) * (t >= 42) + poly(t, 2, raw = TRUE) * (t >= 81), data = ds))

tbl_coef <- function(mod, ...) {
  out <- mod |>
    stats::coef() |>
    tibble::enframe(name = "variable", value = "value") 
  
  deg_poly <- out$variable |>
    stringr::str_extract(pattern = "poly\\(t, [0-9]+, raw = TRUE\\)") |>
    stringr::str_extract("[0-9]+") |>
    as.integer()
  if (all(is.na(deg_poly))) {
    deg_poly <- 0
  } else {
    deg_poly <- max(deg_poly, na.rm = TRUE)
  }
  
  regions <- out$variable |>
    stringr::str_extract(pattern = "t >= [0-9]+") |>
    unique()
  
  if ((deg_poly + 1) * length(regions) != nrow(out)) {
    stop(
      paste(
        "Spline has degree", deg_poly, "over", length(regions), 
        "regions, but", nrow(out), "parameters were fit!"
      )
    )
  }
  
  # fix for polynomials of degree 1
  if (deg_poly == 1) {
    out <- out |>
      dplyr::mutate(
        variable = stringr::str_replace(variable, "TRUE\\)", "TRUE\\)1")
      )
  }
  
  out |>
    dplyr::mutate(
      region = stringr::str_extract(variable, pattern = "t >= [0-9]+") ,
      poly_k = stringr::str_extract(variable, pattern = "poly\\(t, [0-9]+, raw = TRUE\\)[0-9]+"),
      k = as.integer(stringr::str_extract(poly_k, "[0-9]+$")),
      k = ifelse(is.na(k), 0, k)
    ) |>
    dplyr::select(region, k, value) |>
    tidyr::pivot_wider(names_from = "k", values_from = "value", names_prefix = "param_beta") |>
    dplyr::rename(param_mu = param_beta0) |>
    dplyr::mutate(
      param_mu = cumsum(param_mu)
    )
}
