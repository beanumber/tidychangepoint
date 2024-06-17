test_that("changepoint works", {
  x <- segment(DataCPSim, method = "pelt")
  expect_s3_class(x, "tidycpt")
  expect_s4_class(x$segmenter, "cpt")
  expect_s3_class(as.ts(x), "ts")
  expect_s3_class(augment(x), "grouped_ts")
  expect_s3_class(tidy(x), "tbl_df")
  expect_s3_class(glance(x), "tbl_df")
  expect_type(changepoints(x), "integer")
  expect_s3_class(logLik(x$segmenter), "logLik")
  expect_type(fitness(x$segmenter), "double")
  expect_equal(names(fitness(x$segmenter)), "MBIC")
  expect_gt(fitness(x), changepoint::pen.value(x$segmenter))
#  expect_equal(fitness(x), MBIC(x$model))
  expect_true(is_segmenter(x$segmenter))
  expect_true(is_model(x$model))
  
  x <- segment(DataCPSim, method = "binseg")
  expect_s3_class(x, "tidycpt")
  expect_s4_class(x$segmenter, "cpt")
  expect_s3_class(as.ts(x), "ts")
  expect_s3_class(augment(x), "grouped_ts")
  expect_s3_class(tidy(x), "tbl_df")
  expect_s3_class(glance(x), "tbl_df")
  expect_type(changepoints(x), "integer")
})

test_that("changepoint regions match", {
  skip()
#  library(tidychangepoint)
  y <- segment(DataCPSim, method = "pelt", penalty = "BIC")
  y$segmenter@param.est
  y$model$region_params
  
  DataCPSim |>
    tibble::as_tibble() |>
    dplyr::mutate(
      region = rep(1:4, times = diff(c(1, changepoints(y), nobs(y$segmenter) + 1))),
      id = row_number()
    ) |>
    dplyr::group_by(region) |>
    dplyr::summarize(
      N = dplyr::n(), first = min(id), last = max(id), mean = mean(value), var = var(value)
    )
  
  # https://github.com/beanumber/tidychangepoint/issues/99
  expect_equal(
    as.double(logLik(y$segmenter)), 
    as.double(logLik(y$model))
  )
  expect_equal(unname(fitness(y)), BIC(y$model))
})
