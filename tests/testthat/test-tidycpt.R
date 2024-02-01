test_that("tidycpt works", {
  x <- segment(DataCPSim, method = "cpt-pelt")
  expect_s3_class(x, "tidycpt")
  expect_s3_class(as.ts(x), "ts")
  expect_s3_class(augment(x), "grouped_ts")
  expect_s3_class(tidy(x), "tbl_df")
  expect_s3_class(glance(x), "tbl_df")
  expect_type(changepoints(x), "integer")
  expect_s3_class(plot(x), "gg")
  
  z <- segment(DataCPSim, method = "cpt-manual", cpts = c(365, 826))
  expect_s3_class(z, "tidycpt")
  expect_s3_class(as.ts(z), "ts")
  expect_s3_class(augment(z), "grouped_ts")
  expect_s3_class(tidy(z), "tbl_df")
  expect_equal(nrow(tidy(z)), 3)
  expect_s3_class(glance(z), "tbl_df")
  expect_type(changepoints(z), "integer")
  expect_equal(length(changepoints(z)), 2)
  expect_s3_class(plot(z), "gg")
})

test_that("utils works", {
  x <- DataCPSim
  expect_true(all(exceedances(x) %in% 1:length(x)))
  
  y <- pad_tau(x, 826)
  expect_true(0 %in% y)
  expect_true(826 %in% y)
  expect_true(length(x) %in% y)
  
  z <- cut_inclusive(x, y)
  expect_equal(length(z), length(x))
  expect_type(levels(z), "character")
  expect_length(levels(z), 2)
  
  z <- cut_inclusive(1:length(x), y)
  expect_equal(length(z), length(x))
  expect_type(levels(z), "character")
  expect_length(levels(z), 2)
})

