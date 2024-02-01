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
  m <- media_acumulada(x = as.ts(lista_AG), tau, theta)
  expect_equal(length(m), length(exceedances(as.ts(lista_AG))))
  
  theta <- data.frame(
    alpha = c(1, 1), 
    beta = c(1, 1e8)
  )
  tau <- 50
  
  y <- c(rep(75, times = 50), rep(25, times = 50)) |>
    as.ts()
  plot(y)
  
#  z <- segment_gbmdl(y, param)
  m <- media_acumulada(x = y, tau, theta)
  m
  plot_exceedances(y)
})

