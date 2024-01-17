test_that("revisor_param works", {
  skip()
  expect_output(revisor_param(param), "valid")
  expect_error(revisor_param("nothing"))
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

test_that("cpt_list works", {
  expect_s3_class(lista_AG, "cpt_list")
  expect_type(lista_AG, "list")
  expect_equal(lista_AG$minimo_BMDL, cpt_best_bmdl(lista_AG))
  expect_equal(lista_AG$valor_BMDL_minimo, cpt_best_bmdl_string(lista_AG))
  chromosome_best(lista_AG)
  expect_equal(lista_AG$cromosoma_minimo_BMDL[3:6], cpt_best(lista_AG))
})

test_that("exceedances works", {
  expect_identical(DataCPSimRebases, exceedances(DataCPSim))
})
