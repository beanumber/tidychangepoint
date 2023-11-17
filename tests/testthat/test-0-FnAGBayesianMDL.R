test_that("multiplication works", {
  expect_output(revisor_param(param), "valid")
  expect_error(revisor_param("nothing"))
})
