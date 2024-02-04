test_that("revisor_param works", {
  expect_output(revisor_param(param), "valid")
  expect_error(revisor_param("nothing"))
  # Validador de la dimensiones de cosas de distribucion a priori
  if (param$rf_type %in% c("W", "MO", "GO")) {
    dim_a_priori <- 2
  } else {
    dim_a_priori <- 3
  }
  
  expect_equal(length(param$vec_dist_a_priori), dim_a_priori)
  expect_equal(length(param$vec_dist_a_priori), nrow(param$mat_phi))
  #  expect_equal(length(param$vec_dist_a_priori), nrow(param$mat_low_upp))
  #  expect_equal(dim(param$mat_phi), dim(param$mat_low_upp))
  #  expect_equal(dim(param$initial_val_optim), dim_a_priori)
  
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
  m <- cdf_exceedances_est(exceedances(as.ts(lista_AG)), tau, theta, length(as.ts(lista_AG)))
  expect_equal(length(m), length(exceedances(as.ts(lista_AG))))
})


test_that("parameter fitting works", {
  # Example 1
  y <- test_set(n = 1, seed = 123)
  plot(y)
  tau <- attr(y, "cpt_true")
  theta <- fit_nhpp(y, tau)
  expect_lt(abs(theta$alpha[1] - 1), 0.05)
  
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
    theta[1, 2:3] |> unlist() |> unname()
  )
  
  expect_equal(
    fit_nhpp_region(t = z[[2]], tau_left = tau, tau_right = length(y))$par,
    theta[2, 2:3] |> unlist() |> unname()
  )

  plot_mcdf(segment(y, method = "cpt-manual", cpts = tau))
})
