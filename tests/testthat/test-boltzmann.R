test_that("GABolztmann works", {
  skip()
  x <- segment(DataCPSim, method = "boltzmann", num_generations = 2)
  expect_s3_class(x, "tidycpt")
  expect_s3_class(x$segmenter, "seg_default")
})

test_that("RandomKeys works", {
  expect_lte(length(RandomKeys(5)), 5)
  expect_lte(length(RandomKeys(10)), 10)
})