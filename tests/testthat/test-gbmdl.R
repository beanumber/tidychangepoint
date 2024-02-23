test_that("gbmdl works", {
  x <- lista_AG
  expect_s3_class(x, "tidycpt")
  expect_s3_class(x$segmenter, "cpt_gbmdl")
  expect_s3_class(as.ts(x), "ts")
  expect_s3_class(augment(x), "grouped_ts")
  expect_s3_class(tidy(x), "tbl_df")
  expect_s3_class(glance(x), "tbl_df")
  expect_type(changepoints(x), "integer")
  expect_type(nobs(x), "integer")
  expect_s3_class(plot(x), "gg")
  
  expect_s3_class(logLik(x), "logLik")
  expect_type(AIC(x), "double")
  expect_type(BIC(x), "double")
})

test_that("params works", {
  # Validador de la dimensiones de cosas de distribucion a priori
  x <- cpt_gbmdl(DataCPSim)
  if (x$params$nhpp_dist %in% c("W", "MO", "GO")) {
    dim_a_priori <- 2
  } else {
    dim_a_priori <- 3
  }
  
  expect_equal(length(x$params$vec_dist_a_priori), dim_a_priori)
  expect_equal(length(x$params$vec_dist_a_priori), nrow(x$params$mat_phi))
  #  expect_equal(length(param$vec_dist_a_priori), nrow(param$mat_low_upp))
  #  expect_equal(dim(param$mat_phi), dim(param$mat_low_upp))
  #  expect_equal(dim(param$initial_val_optim), dim_a_priori)
  
  # Validador de entradas de mat_low_upp
  # expect_true(!all(param$mat_low_upp[, 2] - param$mat_low_upp[, 1] > 0))
  
  
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

test_that("cpt_gbmdl works", {
  x <- lista_AG$segmenter
  expect_s3_class(x, "cpt_gbmdl")
  expect_type(x, "list")
  expect_equal(min(x$candidates$bmdl), BMDL(x))
})

test_that("regions works", {
  tau <- changepoints(lista_AG)
  expect_equal(tau, changepoints(lista_AG$segmenter))
  expect_false(0 %in% tau)
  expect_false(length(lista_AG) %in% tau)
  y <- split_by_tau(as.ts(lista_AG), tau)
  expect_equal(length(y), length(tau) + 1)
})
