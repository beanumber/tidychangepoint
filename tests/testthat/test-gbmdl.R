test_that("gbmdl works", {
  # x <- segment(DataCPSim, method = "cpt-gbmdl")
  x <- list(segmenter = lista_AG)
  class(x$segmenter) <- c("cpt_gbmdl", class(x))
  class(x) <- "tidycpt"
  expect_s3_class(x, "tidycpt")
  expect_s3_class(x$segmenter, "cpt_gbmdl")
  expect_s3_class(as.ts(x), "ts")
  expect_s3_class(augment(x), "grouped_ts")
  expect_s3_class(tidy(x), "tbl_df")
  expect_s3_class(glance(x), "tbl_df")
  expect_type(changepoints(x), "integer")
})
