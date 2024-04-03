test_that("gbmdl works", {
  x <- segment(DataCPSim, method = "gbmdl", num_generations = 3)
  expect_s3_class(x, "tidycpt")
  expect_s3_class(x$segmenter, "cpt_gbmdl")
  expect_s3_class(as.ts(x), "ts")
  expect_s3_class(augment(x), "grouped_ts")
  expect_s3_class(tidy(x), "tbl_df")
  expect_s3_class(glance(x), "tbl_df")
  expect_type(changepoints(x), "integer")
  expect_type(nobs(x), "integer")
  expect_s3_class(plot(x), "gg")
  
  expect_s3_class(logLik(x$nhpp), "logLik")
  expect_equal(min(x$segmenter$candidates$BMDL), BMDL(x$nhpp))
})

test_that("params works", {
  # Validador de la dimensiones de cosas de distribucion a priori
  x <- segment(DataCPSim, method = "gbmdl", num_generations = 2)
  if (x$segmenter$params$nhpp_dist %in% c("W", "MO", "GO")) {
    dim_a_priori <- 2
  } else {
    dim_a_priori <- 3
  }
  
  expect_equal(length(x$segmenter$params$vec_dist_a_priori), dim_a_priori)
  expect_equal(length(x$segmenter$params$vec_dist_a_priori), nrow(x$segmenter$params$mat_phi))
  #  expect_equal(length(param$vec_dist_a_priori), nrow(param$mat_low_upp))
  #  expect_equal(dim(param$mat_phi), dim(param$mat_low_upp))
  #  expect_equal(dim(param$initial_val_optim), dim_a_priori)
  
  # Validador de entradas de mat_low_upp
  # expect_true(!all(param$mat_low_upp[, 2] - param$mat_low_upp[, 1] > 0))
})

