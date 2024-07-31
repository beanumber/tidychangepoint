test_that("lmshift works", {
  x <- as.ts(DataCPSim)
  expect_type(exceedances(x), "integer")
  
  expect_s3_class(fit_meanshift_norm(CET, tau = NULL), "mod_cpt")
  expect_error(fit_meanshift_norm(CET, tau = NA))
  expect_equal(changepoints(fit_meanshift_norm(CET, tau = NULL)), integer())
  
  cpts <- c(1700, 1739, 1988)
  ids <- time2tau(cpts, as_year(time(CET)))
  
  x <- fit_meanshift_norm(CET, tau = ids)
  expect_s3_class(x, "mod_cpt")
  expect_true("sigma_hatsq" %in% names(x$model_params))
  expect_false("phi_hat" %in% names(x$model_params))
  expect_true("region" %in% names(x$region_params))

  y <- fit_meanshift_norm_ar1(CET, tau = ids)
  expect_s3_class(y, "mod_cpt")
  expect_true("sigma_hatsq" %in% names(y$model_params))
  expect_true("phi_hat" %in% names(y$model_params))
  expect_gt(y$model_params["phi_hat"], 0)
  
  z <- fit_lmshift(CET, tau = ids)
  expect_true(all(abs(fitted(z) - fitted(x)) < 0.000000001))
  expect_equal(model_variance(x), model_variance(z))
  expect_equal(coef(x), coef(z))
  expect_equal(x$model_params["sigma_hatsq"], z$model_params["sigma_hatsq"])
  expect_equal(deg_free(x), deg_free(z))
  expect_true("region" %in% names(z$region_params))
  
  w <- fit_lmshift_ar1(CET, tau = ids)
  expect_true(all(abs(fitted(w) - fitted(y)) < 0.000000001))
  expect_equal(model_variance(y), model_variance(w))
  expect_equal(coef(y), coef(w))
  expect_equal(deg_free(y), deg_free(w))
  
  x <- fit_meanvar(CET, tau = c(42, 330))
  expect_true("param_sigma_hatsq" %in% names(x$region_params))
  expect_true("region" %in% names(x$region_params))
  
})
  
test_that("values match", {  
  cpts <- c(1700, 1739, 1988)
  ids <- time2tau(cpts, as_year(time(CET)))
  
  trend_wn <- fit_trendshift(CET, tau = ids)
  expect_equal(round(trend_wn$model_params[["sigma_hatsq"]], 3), 0.291)
  expect_equal(round(as.numeric(logLik(trend_wn)), 2), -290.02)
  expect_equal(round(BIC(trend_wn), 2), 650.74)
  expect_equal(round(MDL(trend_wn), 2), 653.07)

  trend_ar1 <- fit_trendshift_ar1(CET, tau = ids)
  
  resid_wn <- residuals(trend_wn)
  resid_ar1 <- residuals(trend_ar1)
  expect_gt(var(resid_wn - resid_ar1), 0)
#  tidychangepoint:::autoregress_errors(trend_wn) |>
#    residuals()
  
  expect_equal(round(trend_ar1$model_params[["sigma_hatsq"]], 3), 0.290)
  
# https://github.com/beanumber/tidychangepoint/issues/73
  expect_equal(round(as.numeric(logLik(trend_ar1))), round(-288.80))
  expect_equal(round(BIC(trend_ar1)), round(654.19))
  expect_equal(floor(MDL(trend_ar1)), floor(656.52))
  expect_equal(round(trend_ar1$model_params[["phi_hat"]], 3), 0.058)
  
  spline_wn <- fit_lmshift(CET, tau = ids, deg_poly = 4)
  expect_equal(ncol(coef(spline_wn)), 6)
  expect_lt(model_variance(spline_wn), model_variance(trend_wn))
  
  # truncated series
  CET_trunc <- CET['1772-01-01/'] 
  tau <- time2tau(1987, as_year(time(CET_trunc)))
  
  trend_wn_trunc <- fit_lmshift(CET_trunc, tau = tau, deg_poly = 2)
  trend_wn_trunc$sigma_hatsq
  logLik(trend_wn_trunc)
  BIC(trend_wn_trunc)
  MDL(trend_wn_trunc) + 2 * log(nobs(trend_wn_trunc))
  
  trend_ar1_trunc <- fit_lmshift_ar1(CET_trunc, tau = tau, deg_poly = 1)
  expect_equal(round(model_variance(trend_ar1_trunc), 3), 0.305)
  expect_equal(round(trend_ar1_trunc$model_params[["phi_hat"]], 3), 0.073)
  logLik(trend_ar1_trunc)
  BIC(trend_ar1_trunc)
  MDL(trend_ar1_trunc) + 2 * log(nobs(trend_ar1_trunc))
})

test_that("model performance", {
  skip()
  bench::mark(
    m1 = fit_meanshift_norm(CET, tau = ids), 
    m2 = fit_meanshift2(CET, ids), 
    lm = fit_lmshift(CET, ids), 
    check = FALSE
  )
}) 