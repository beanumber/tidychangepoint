test_that("generics works", {
  theta <- fit_nhpp(DataCPSim, tau = 826)
  
  expect_true(all(c("param_alpha", "param_beta") %in% names(theta$region_params)))
  expect_type(changepoints(theta), "integer")
  expect_type(exceedances(theta), "integer")
  expect_s3_class(logLik(theta), "logLik")
  expect_type(BMDL(theta), "double")
  expect_type(MBIC(theta), "double")
  expect_type(MDL(theta), "double")
  expect_s3_class(glance(theta), "tbl_df")
  
  m <- mcdf(theta)
  expect_equal(length(m), length(exceedances(theta)))
  
  x <- fit_nhpp(DataCPSim, tau = NULL)
  expect_equal(deg_free(x), 3)
  expect_equal(AIC(x), as.double(2 * deg_free(x) - 2 * logLik(x)))
  expect_equal(BIC(x), as.double(log(nobs(x)) * deg_free(x) - 2 * logLik(x)))
  expect_equal(MBIC(x), as.double(-2 * logLik(x)))
  expect_equal(MDL(x), as.double(-2 * logLik(x)))
  expect_equal(BMDL(x), -2 * sum(x$region_params[["logPost"]]))
  expect_s3_class(glance(x), "tbl_df")
  
  
  y <- fit_nhpp(DataCPSim, tau = 826, threshold = 200)
  expect_true(all(mcdf(y) < length(m)))
})

test_that("threshold works", {
  x <- fit_nhpp(DataCPSim, tau = 826)
  expect_equal(length(exceedances(x)), 360)
  expect_equal(length(exceedances(x)), length(mcdf(x)))
  
  y <- fit_nhpp(DataCPSim, tau = 826, threshold = 200)
  expect_lt(length(exceedances(y)), 360)
  expect_equal(length(exceedances(y)), length(mcdf(y)))
  expect_equal(y$model_params[["threshold"]], 200)
})

test_that("BMDL works", {
  y <- test_set(n = 1, seed = 123)
  seg <- segment(y, method = "pelt")
  nhpp <- fit_nhpp(y, changepoints(seg))
  expect_s3_class(logLik(nhpp), "logLik")
  expect_type(BMDL(nhpp), "double")

  expect_error(fit_nhpp(y, c(0, 500, 2000))$region_params)
  expect_s3_class(fit_nhpp(y, 500)$region_params, "tbl")
  
  z <- segment(DataCPSim, method = "null")
  nhpp <- fit_nhpp(as.ts(z), changepoints(z))
  expect_equal(BMDL(nhpp), - 2 * sum(nhpp$region_params[["logPost"]]))
})

test_that("parameter fitting works", {
  # Example 1
  y <- test_set(n = 1, seed = 123)
  tau <- attr(y, "cpt_true")
  theta <- fit_nhpp(y, tau)
  diagnose(segment(y, method = "manual", tau = tau))
  expect_lt(abs(theta$region_params[["param_alpha"]][1] - 1), 0.05)
})
