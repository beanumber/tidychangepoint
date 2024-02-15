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
  expect_equal(
    log_prior_region_weibull(theta = c(0.5, 2)),
    Bloq_LogPrio_NHPP(c("Gamma", "Gamma"), theta = c(0.5, 2), mat_phi = matrix(c(1, 3, 2, 1.2), ncol = 2))
  )
  
  1:200 / 100 |> 
    purrr::map_dbl(~log_prior_region_weibull(theta = c(.x, 2))) |>
    expect_type("double")
  
  1:1000 / 100 |> 
    purrr::map_dbl(~log_prior_region_weibull(theta = c(0.5, .x))) |>
    expect_type("double")
  
  expect_equal(
    D_Bloq_LogVero_NHPP(exceedances(DataCPSim), 0, 575, theta = c(0.5, 2), nhpp_dist = "W"),
    D_log_likelihood_region_weibull(exceedances(DataCPSim), 0, 575, theta = c(0.5, 2))
  )
  expect_equal(
    D_log_prior_region_weibull(theta = c(0.5, 2)),
    D_Bloq_LogPrio_NHPP(c("Gamma", "Gamma"), theta = c(0.5, 2), mat_phi = matrix(c(1, 3, 2, 1.2), ncol = 2))
  )
  
  fit_nhpp_region(exceedances(lista_AG), 0, 575)
  fit_nhpp_region_alt(exceedances(lista_AG), 0, 575)

  # Example 1
  y <- test_set(n = 1, seed = 123)
  plot(y)
  tau <- attr(y, "cpt_true")
  theta <- fit_nhpp(y, tau)
  diagnose(segment(y, method = "cpt-manual", cpts = tau))
  
  theta_true <- c(0.972236, 0.83897783)
  log_prior_region_weibull(theta = theta_true)
  exc <- exceedances(y)
  log_likelihood_region_weibull(exc[exc <= tau], 0, tau, theta = theta_true)
  
  expect_equal(-1000, log_likelihood_region_weibull(exc, 0, 1000, theta = c(1, 1)))
  expect_equal(-Inf, log_likelihood_region_weibull(exc, 0, 1000, theta = c(0, 1)))
  log_likelihood_region(y, 0, 1000, theta = c(exp(1), 1))
  log_likelihood_region(y, 0, 1000, theta = c(1, 10000))
  expect_equal(-length(exceedances) * (1/exp(1) + 1), log_likelihood_region_weibull(exc, 0, 1000, theta = c(1, exp(1))))
  
  expect_equal(
    Bloq_LogVero_NHPP(exc, 0, 1000, theta = c(0.5, 2), nhpp_dist = "W"),
    log_likelihood_region_weibull(exc, 0, 1000, theta = c(0.5, 2))
  )
  
  expect_equal(
    log_likelihood_region(y, 0, 1000, theta = c(1, 1)),
    log_likelihood_region(y, 0, 416, theta = c(1, 1)) + log_likelihood_region(y, 416, 1000, theta = c(1, 1))
  )
  
  log_likelihood_region(y, 0, 416, theta = c(1, 1)) + log_likelihood_region(y, 416, 1000, theta = c(1.1, 1000))
  
  f <- function(x) log_likelihood_region(y, 0, 1000, theta = c(x, 1))
  g <- function(x) log_likelihood_region(y, 0, 1000, theta = c(1, x))
  h <- function(x) {
    log_likelihood_region(y, 0, x, theta = c(1, 1)) +
      log_likelihood_region(y, x, 1000, theta = c(1, 1))
  }
  
  1:100 |>
    purrr::map_dbl(f)
  
  1:100 |>
    purrr::map_dbl(g)
  
  1:1000 |>
    purrr::map_dbl(h)
  
  
  log_likelihood_region_weibull(DataCPSim, 0, 575, theta = c(0.5, 2))
})
