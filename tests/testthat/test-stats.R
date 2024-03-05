test_that("GA works", {
  x <- as.ts(DataCPSim)
  expect_type(exceedances(x), "integer")
  y <- logLik(x, loc.ind = tau2binary(826, n = length(x)))
  expect_s3_class(y, "logLik")
  expect_type(BIC(y), "double")
  expect_type(MDL(y), "double")
})
