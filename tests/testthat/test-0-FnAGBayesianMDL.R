test_that("revisor_param works", {
  skip()
  expect_output(revisor_param(param), "valid")
  expect_error(revisor_param("nothing"))
})


test_that("GABolztmann works", {
  skip()
  x <- GABoltzmannMutation2(param)
  expect_s3_class(x, "list")
  expect_equal(length(x), 2)
})

test_that("RandomKeys works", {
  expect_lte(length(RandomKeys(5)), 5)
  expect_lte(length(RandomKeys(10)), 10)
})

test_that("cpt_list works", {
  x <- lista_AG$segmenter
  expect_s3_class(x, "cpt_gbmdl")
  expect_type(x, "list")
  expect_equal(min(x$vec_min_BMDL), cpt_best_bmdl(x))
  expect_true(all(cpt_best(x) %in% chromosome_best(x)))
})

test_that("exceedances works", {
  expect_identical(DataCPSimRebases, exceedances(DataCPSim))
})

test_that("regions works", {
  tau <- changepoints(lista_AG)
  expect_equal(tau, cpt_best(lista_AG$segmenter))
  expect_false(0 %in% tau)
  expect_false(length(as.ts(lista_AG)) %in% tau)
  y <- split_by_tau(as.ts(lista_AG), tau)
  expect_equal(length(y), length(tau) + 1)
})

test_that("mcddf works", {
  tau <- changepoints(lista_AG)
  theta <- cpt_best_params(lista_AG$segmenter)
  m <- media_acumulada(exceedances(as.ts(lista_AG)), tau, theta, length(as.ts(lista_AG)))
  expect_equal(length(m), length(exceedances(as.ts(lista_AG))))
})


test_that("parameter fitting works", {
  # Example 1
  y <- c(rep(75, times = 50), rep(25, times = 50))
  tau <- 50
  theta <- fit_nhpp(y, tau, param = param)
  expect_lt(abs(theta$alpha[1] - 1), 0.12)
  
  m <- media_acumulada(exceedances(y), tau, theta, length(y))
  expect_equal(media_acumulada(0, tau, theta, length(y)), 0)
  expect_lt(abs(media_acumulada(50, tau, theta, length(y)) - 50), 1)
  expect_lt(abs(media_acumulada(100, tau, theta, length(y)) - 50), 3)
  
  plot_confint2(y, tau, theta = theta)
  
  # Example 2
  y <- c(rnorm(50, mean = 60, sd = 5), rnorm(50, mean = 40, sd = 5))
  z <- split(exceedances(y), cut_inclusive(exceedances(y), pad_tau(tau, length(y))))

  theta <- fit_nhpp(y, tau, param = param)
  theta
  
  expect_equal(
    fit_nhpp_region(t = z[[1]], tau_left = 0, tau_right = tau)$par,
    theta[1, 2:3] |> unlist() |> unname()
  )
  
  expect_equal(
    fit_nhpp_region(t = z[[2]], tau_left = tau, tau_right = length(y))$par,
    theta[2, 2:3] |> unlist() |> unname()
  )

  plot_confint2(y, tau, theta = theta)
})
