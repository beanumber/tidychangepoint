test_that("generics works", {
  theta <- fit_nhpp(DataCPSim, tau = 826)
  
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
  expect_equal(BMDL(x), -2 * sum(x$log_posterior))
  expect_s3_class(glance(x), "tbl_df")
  
  
  y <- fit_nhpp(DataCPSim, tau = 826, threshold = 200)
  expect_lt(length(mcdf(y)), length(m))
})


test_that("BMDL works", {
  y <- test_set(n = 1, seed = 123)
  seg <- segment(y, method = "pelt")
  expect_s3_class(logLik(seg), "logLik")
  expect_type(BMDL(seg), "double")

  expect_equal(fit_nhpp(y, c(0, 500, 2000)), fit_nhpp(y, 500))
  
  z <- segment(DataCPSim, method = "null")
  expect_equal(BMDL(z), - 2 * sum(z$nhpp$log_posterior))
})

test_that("parameter fitting works", {
  # Example 1
  y <- test_set(n = 1, seed = 123)
  tau <- attr(y, "cpt_true")
  theta <- fit_nhpp(y, tau)
  diagnose(segment(y, method = "manual", cpts = tau))
  expect_lt(abs(theta$alpha[1] - 1), 0.05)
})
