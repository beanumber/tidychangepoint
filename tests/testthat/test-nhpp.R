
test_that("mcddf works", {
  tau <- changepoints(lista_AG)
  theta <- cpt_best_params(lista_AG$segmenter)
  m <- cdf_exceedances_est(exceedances(lista_AG), tau, theta, length(lista_AG))
  expect_equal(length(m), length(exceedances(lista_AG)))
})


test_that("parameter fitting works", {
  # Example 1
  y <- test_set(n = 1, seed = 123)
  plot(y)
  tau <- attr(y, "cpt_true")
  theta <- fit_nhpp(y, tau)
  plot_mcdf(segment(y, method = "cpt-manual", cpts = tau))
  expect_lt(abs(theta$alpha[1] - 1), 0.05)
  
  expect_equal(-1000, log_likelihood_region(y, 0, 1000, theta = c(1, 1)))
  expect_equal(-Inf, log_likelihood_region(y, 0, 1000, theta = c(0, 1)))
  log_likelihood_region(y, 0, 1000, theta = c(exp(1), 1))
  log_likelihood_region(y, 0, 1000, theta = c(1, 10000))
  expect_equal(-1000 * (1/exp(1) + 1), log_likelihood_region(y, 0, 1000, theta = c(1, exp(1))))
  
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
  
  
  m <- cdf_exceedances_est(exceedances(y), tau, theta, length(y))
  expect_equal(cdf_exceedances_est(0, tau, theta, length(y)), 0)
  expect_lt(abs(cdf_exceedances_est(tau, tau, theta, length(y)) - tau), 2)
  expect_lt(abs(cdf_exceedances_est(length(y), tau, theta, length(y)) - tau), 3)
  
  plot_mcdf(segment(y, method = "cpt-manual", cpts = tau))
  
  # Example 2
  y <- test_set(n = 1, seed = 456)
  plot(y)
  tau <- attr(y, "cpt_true")
  z <- split(exceedances(y), cut_inclusive(exceedances(y), pad_tau(tau, length(y))))
  
  theta <- fit_nhpp(y, tau)
  theta
  
  expect_equal(
    fit_nhpp_region(t = z[[1]], tau_left = 0, tau_right = tau)$par,
    theta[1, c("alpha", "beta")] |> unlist() |> unname()
  )
  
  expect_equal(
    fit_nhpp_region(t = z[[2]], tau_left = tau, tau_right = length(y))$par,
    theta[2, c("alpha", "beta")] |> unlist() |> unname()
  )
  
  plot_mcdf(segment(y, method = "cpt-manual", cpts = tau))
})
