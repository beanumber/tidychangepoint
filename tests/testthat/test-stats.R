test_that("lmshift works", {
  x <- as.ts(DataCPSim)
  expect_type(exceedances(x), "integer")
  
  expect_s3_class(fit_meanshift(CET, tau = NULL), "meanshift")
  expect_s3_class(fit_meanshift(CET, tau = NA), "meanshift")
  expect_equal(fit_meanshift(CET, tau = NA)$tau, NA)
  
  cpts <- c(1700, 1739, 1988)
  ids <- time2tau(cpts, substr(time(CET), 1, 4))
  
  x <- fit_meanshift(CET, tau = ids)
  expect_s3_class(x, "meanshift")
  expect_equal(x$phi_hat, 0)
  expect_false(x$ar1)
  
  tidychangepoint:::autoregress_errors(x)
  
  y <- fit_meanshift_ar1(CET, tau = ids)
  expect_s3_class(y, "meanshift")
  expect_gt(y$phi_hat, 0)
  expect_true(y$ar1)
  
  z <- fit_lmshift(CET, tau = ids)
  expect_true(all(abs(z$fitted.values - x$fitted.values) < 0.000000001))
  expect_equal(x$sigma_hatsq, z$sigma_hatsq)
  expect_equal(deg_free(x), deg_free(z))
  
  w <- fit_lmshift(CET, tau = ids, ar1 = TRUE)
  expect_true(all(abs(w$fitted.values - y$fitted.values) < 0.000000001))
  expect_equal(y$sigma_hatsq, w$sigma_hatsq)
  expect_equal(deg_free(y), deg_free(w))
  
  trend_wn <- fit_lmshift(CET, tau = ids, trends = TRUE)
  expect_equal(round(as.numeric(logLik(trend_wn)), 2), -290.02)
  expect_equal(round(BIC(logLik(trend_wn)), 2), 650.74)
  expect_equal(round(MDL(logLik(trend_wn)), 2), 653.07)
  
  trend_ar1 <- fit_lmshift(CET, tau = ids, trends = TRUE, ar1 = TRUE)
  expect_equal(round(as.numeric(logLik(trend_ar1)), 2), -288.80)
  expect_equal(round(BIC(trend_ar1), 2), 654.19)
  expect_equal(MDL(logLik(trend_ar1)), 656.52)
})
