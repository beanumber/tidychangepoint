#' Regression-based model fitting
#' @export
#' @examples
#' tau <- c(365, 826)
#' mod <- fit_lm(DataCPSim, tau)
#' logLik(mod)
#' deg_free(mod)
#' 
#' cpts = c(1700, 1739, 1988)
#' ids <- match(cpts, lubridate::year(index(CET)))
#' mod <- fit_lm(CET, tau = ids)
#' 
#' mod <- fit_lm(CET, tau = ids, trends = TRUE)
#' logLik(mod)
#' deg_free(mod)

fit_lm <- function(x, tau, trends = FALSE, ...) {
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

