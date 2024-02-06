test_that("tidycpt works", {
  x <- segment(DataCPSim, method = "cpt-pelt")
  expect_s3_class(x, "tidycpt")
  expect_s3_class(as.ts(x), "ts")
  expect_s3_class(augment(x), "grouped_ts")
  expect_s3_class(tidy(x), "tbl_df")
  expect_true(all(c("alpha", "beta", "region") %in% names(tidy(x))))
  expect_s3_class(glance(x), "tbl_df")
  expect_type(changepoints(x), "integer")
  expect_s3_class(plot(x), "gg")
  expect_s3_class(diagnose(x), "patchwork")
  
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
  expect_s3_class(diagnose(z), "patchwork")
})

test_that("utils works", {
  x <- DataCPSim
  expect_true(all(exceedances(x) %in% 1:length(x)))
  
  tau <- 826
  y <- pad_tau(tau, length(x))
  expect_true(0 %in% y)
  expect_true(all(tau %in% y))
  expect_true(length(x) %in% y)
  expect_equal(unpad_tau(y), tau)
  expect_false(0 %in% unpad_tau(y))
  expect_true(all(tau %in% y))
  expect_false(length(x) %in% unpad_tau(y))
  
  z <- cut_inclusive(x, y)
  expect_equal(length(z), length(x))
  expect_type(levels(z), "character")
  expect_length(levels(z), 2)
  
  z <- cut_inclusive(1:length(x), y)
  expect_equal(length(z), length(x))
  expect_type(levels(z), "character")
  expect_length(levels(z), 2)
})

test_that("penalties work", {
  mat_cp <- sim_k_cp_BMDL(DataCPSim)
  
  Bayesaian_MDL_k_cp(mat_cp, exceedances(DataCPSim))
  mat_cp |>
    mat_cp_2_list() |>
    purrr::map_dbl(bmdl, x = DataCPSim)
  
  tau <- chromo2tau(mat_cp[1,])
  Bayesaian_MDL_1_cp(mat_cp[1,], exceedances(DataCPSim))
  bmdl(DataCPSim, chromo2tau(mat_cp[1,]))
  
  # log-posterior
  extrae_mat_MAP(mat_cp[1,], DataCPSim)
  fit_nhpp(DataCPSim, tau)
  
  penalty_mdl(pad_tau(tau, n = length(as.ts(DataCPSim))))
  penalization_MDL(mat_cp[1,], "W", N = length(exceedances(DataCPSim)))
  
  expect_equal(penalty_mdl(pad_tau(0, n = 1)), -Inf)
  expect_equal(penalty_mdl(pad_tau(50, n = 100)), 3 * log(50))
  expect_gt(penalty_mdl(pad_tau(c(25, 50), n = 100)), penalty_mdl(pad_tau(50, n = 100)))
  
  x <- test_set()
  cpt <- attr(x, "cpt_true")
  expect_gt(penalty_mdl(pad_tau(cpt, length(as.ts(x)))), 0)
  
  expect_equal(bmdl(x, 0), -Inf)
  bmdl(x, cpt)
  expect_true(all(purrr::map_dbl(runif(10, max = length(as.ts(x))), bmdl, x = x) >= bmdl(x, cpt)))
})
