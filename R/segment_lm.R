#' Regression-based model fitting
#' @export
#' @examples
#' tau <- c(365, 826)
#' mod <- fit_lm(DataCPSim, tau)
#' logLik(mod)
#' deg_free(mod)
#' 
#' mod <- fit_lm(DataCPSim, tau, trends = TRUE)
#' logLik(mod)
#' deg_free(mod)

fit_lm <- function(x, tau, trends = FALSE, ...) {
  n <- length(x)
  ds <- data.frame(y = x, t = 1:n)
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
  form <- form |>
    stats::as.formula()

  stats::lm(form, data = ds)
}

