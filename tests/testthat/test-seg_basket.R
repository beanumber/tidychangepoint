test_that("seg_basket works", {
  x <- segment(DataCPSim, method = "random")
  expect_s3_class(x, "tidycpt")
  expect_s3_class(x$segmenter, "seg_basket")
  expect_s3_class(as.ts(x), "ts")
  expect_s3_class(augment(x), "grouped_ts")
  expect_s3_class(tidy(x), "tbl_df")
  suppressWarnings(expect_s3_class(glance(x), "tbl_df"))
  expect_type(changepoints(x), "integer")
  expect_true(all(c("logLik", "AIC", "BIC", "MBIC", "MDL") %in% names(x$segmenter$basket)))
  
  expect_true(is_segmenter(x$segmenter))
  expect_true(is_model(x$model))

  expect_s3_class(evaluate_cpts(x$segmenter), "tbl_df")
  expect_s3_class(evaluate_cpts(list(), .data = DataCPSim, model_fn = fit_nhpp), "tbl_df")
  expect_s3_class(evaluate_cpts(tibble::tibble(changepoints = list(826)), .data = DataCPSim, model_fn = fit_nhpp), "tbl_df")
})
