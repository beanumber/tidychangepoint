test_that("weibull works", {
  expect_true(all(iweibull(runif(100), shape = 1, scale = 1) == 1))
  expect_type(iweibull(runif(100), shape = 0.5, scale = 2), "double")
  
  expect_equal(mweibull(0, shape = 1, scale = 1), 0)
  z <- runif(100)
  expect_equal(
    mweibull(z, shape = 0.5, scale = 2),
    # see Note in docs
    -pweibull(z, shape = 0.5, scale = 2, lower = FALSE, log = TRUE)
  )
  
  expect_equal(log_prior_region_weibull(theta = c(0, 2)), -Inf)
  expect_equal(log_prior_region_weibull(theta = c(1, 1)), -4)
  expect_type(log_prior_region_weibull(theta = c(0.5, 2)), "double")
  
  expect_type(D_log_likelihood_region_weibull(exceedances(DataCPSim), 0, 575, theta = c(0.5, 2)), "double")
  expect_type(D_log_prior_region_weibull(theta = c(0.5, 2)), "double")

  expect_equal(-1000, log_likelihood_region_weibull(exceedances(DataCPSim), 0, 1000, theta = c(1, 1)))
  expect_equal(-Inf, log_likelihood_region_weibull(exceedances(DataCPSim), 0, 1000, theta = c(0, 1)))
#  expect_equal(
#    -length(exceedances(DataCPSim)) * (1/exp(1) + 1), 
#    log_likelihood_region_weibull(exceedances(DataCPSim), 0, 1000, theta = c(1, exp(1)))
#  )
})
