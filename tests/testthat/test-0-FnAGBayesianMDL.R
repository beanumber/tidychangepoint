test_that("revisor_param works", {
  expect_output(revisor_param(param), "valid")
  expect_error(revisor_param("nothing"))
})

test_that("RandomKeys works", {
  expect_lte(length(RandomKeys(5)), 5)
  expect_lte(length(RandomKeys(10)), 10)
})
