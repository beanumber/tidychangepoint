test_that("generics works", {
  theta <- fit_nhpp(DataCPSim, tau = 826)
  
  expect_type(changepoints(theta), "integer")
  expect_type(exceedances(theta), "integer")
  expect_s3_class(logLik(theta), "logLik")
  expect_type(BMDL(theta), "double")
  expect_type(MBIC(theta), "double")
  
  m <- mcdf(theta)
  expect_equal(length(m), length(exceedances(theta)))
})


test_that("BMDL works", {
  y <- test_set(n = 1, seed = 123)
  seg <- segment(y, method = "pelt")
  expect_s3_class(logLik(seg), "logLik")
  expect_type(BMDL(seg), "double")

  expect_equal(fit_nhpp(y, c(0, 500, 2000)), fit_nhpp(y, 500))
  
  z <- segment(y, method = "null")
  expect_equal(BMDL(z), -Inf)
})

test_that("parameter fitting works", {
  # Example 1
  y <- test_set(n = 1, seed = 123)
  tau <- attr(y, "cpt_true")
  theta <- fit_nhpp(y, tau)
  diagnose(segment(y, method = "manual", cpts = tau))
  expect_lt(abs(theta$alpha[1] - 1), 0.05)
})
