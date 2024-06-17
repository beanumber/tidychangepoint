test_that("gbmdl works", {
  expect_warning(x <- segment(DataCPSim, method = "coen", num_generations = 3), "deprecated")
  expect_s3_class(x, "tidycpt")
  expect_s3_class(x$segmenter, "cpt_gbmdl")
  expect_s3_class(x$segmenter, "seg_basket")
  expect_s3_class(as.ts(x), "ts")
  expect_s3_class(augment(x), "grouped_ts")
  expect_s3_class(tidy(x), "tbl_df")
  expect_s3_class(glance(x), "tbl_df")
  expect_type(changepoints(x), "integer")
  expect_s3_class(plot(x), "gg")

  expect_true(all(c("logLik", "AIC", "BIC", "MBIC", "MDL") %in% names(x$segmenter$basket)))
  
  expect_true(is_segmenter(as.seg_cpt(as.segmenter(x))))
  expect_true(is_model(as.model(x)))
  
  expect_s3_class(evaluate_cpts(x$segmenter), "tbl_df")
  expect_s3_class(evaluate_cpts(list(), .data = DataCPSim, model_fn = fit_nhpp), "tbl_df")
  expect_s3_class(evaluate_cpts(tibble::tibble(changepoints = list(826)), .data = DataCPSim, model_fn = fit_nhpp), "tbl_df")
  
  expect_s3_class(logLik(x$model), "logLik")
  expect_equal(min(x$segmenter$basket$BMDL), BMDL(x$model))
})

test_that("params works", {
  # Validador de la dimensiones de cosas de distribucion a priori
  expect_warning(x <- segment(DataCPSim, method = "coen", num_generations = 3), "deprecated")
  if (x$segmenter$seg_params$nhpp_dist %in% c("W", "MO", "GO")) {
    dim_a_priori <- 2
  } else {
    dim_a_priori <- 3
  }
  
  expect_equal(length(x$segmenter$seg_params$vec_dist_a_priori), dim_a_priori)
  expect_equal(length(x$segmenter$seg_params$vec_dist_a_priori), nrow(x$segmenter$seg_params$mat_phi))
  #  expect_equal(length(param$vec_dist_a_priori), nrow(param$mat_low_upp))
  #  expect_equal(dim(param$mat_phi), dim(param$mat_low_upp))
  #  expect_equal(dim(param$initial_val_optim), dim_a_priori)
  
  # Validador de entradas de mat_low_upp
  # expect_true(!all(param$mat_low_upp[, 2] - param$mat_low_upp[, 1] > 0))
})

