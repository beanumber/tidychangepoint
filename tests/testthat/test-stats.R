test_that("GA works", {
  x <- as.ts(DataCPSim)
  expect_type(exceedances(x), "integer")
  
  null <- fit_meanshift_ar1(CET, tau = NULL)
  expect_s3_class(null, "meanshift")
    
  cpts <- c(1700, 1739, 1988)
  ids <- time2tau(cpts, lubridate::year(time(CET)))
  trend_wn <- fit_lmshift(CET, tau = ids, trends = TRUE)
  expect_equal(round(as.numeric(logLik(trend_wn)), 2), -290.02)
  expect_equal(round(BIC(logLik(trend_wn)), 2), 650.74)
  expect_equal(MDL(logLik(trend_wn)), 653.07)
  
  trend_ar1 <- fit_lmshift(CET, tau = ids, trends = TRUE, ar1 = TRUE)
  expect_equal(round(as.numeric(logLik(trend_ar1)), 2), -288.80)
  expect_equal(round(BIC(trend_ar1), 2), 654.19)
  expect_equal(MDL(logLik(trend_ar1)), 656.52)
})
