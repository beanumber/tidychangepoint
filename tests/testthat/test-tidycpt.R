test_that("data works", {
  expect_s3_class(bogota_pm, "tbl_ts")
})

test_that("tidycpt works", {
  x <- segment(DataCPSim, method = "cpt-pelt")
  expect_s3_class(x, "tidycpt")
  expect_s3_class(as.ts(x), "ts")
  expect_s3_class(augment(x), "grouped_ts")
  expect_s3_class(tidy(x), "tbl_df")
  expect_true(all(c("alpha", "beta", "region") %in% names(tidy(x))))
  expect_s3_class(glance(x), "tbl_df")
  expect_type(changepoints(x), "integer")
  expect_equal(AIC(x), as.numeric(-2 * logLik(x) + 2 * deg_free(x)))
  expect_equal(BIC(x), as.numeric(-2 * logLik(x) + log(nobs(x)) * deg_free(x)))
  expect_type(BMDL(x), "double")
  expect_s3_class(plot(x), "gg")
  expect_s3_class(diagnose(x), "patchwork")
  
  # AIC, BIC not quite right
  library(tidychangepoint)
  pelt_bic <- segment(DataCPSim, method = "cpt-pelt", penalty = "BIC")
  pelt_bic |>
    glance() |> 
    dplyr::select(dplyr::matches("IC|Lik"))
  
  z <- segment(DataCPSim, method = "cpt-manual", cpts = c(365, 826))
  expect_s3_class(z, "tidycpt")
  expect_s3_class(as.ts(z), "ts")
  expect_s3_class(augment(z), "grouped_ts")
  expect_s3_class(tidy(z), "tbl_df")
  expect_equal(nrow(tidy(z)), 3)
  expect_s3_class(glance(z), "tbl_df")
  expect_type(changepoints(z), "integer")
  expect_equal(length(changepoints(z)), 2)
  expect_equal(AIC(z), as.numeric(-2 * logLik(z) + 2 * deg_free(z)))
  expect_equal(BIC(z), as.numeric(-2 * logLik(z) + log(nobs(z)) * deg_free(z)))
  expect_type(BMDL(z), "double")
  expect_s3_class(plot(z), "gg")
  expect_s3_class(diagnose(z), "patchwork")
  
  expect_s3_class(segment(bogota_pm, method = "cpt-manual", cpts = c(500, 850)), "tidycpt")
  expect_error(segment(bogota_pm, method = "cpt-manual", tau = c(500, 850)), "cpts")
  
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
    purrr::map(fit_nhpp, x = DataCPSim) |>
    purrr::map_dbl(BMDL)
  
  tau <- chromo2tau(mat_cp[1,])
  Bayesaian_MDL_1_cp(mat_cp[1,], exceedances(DataCPSim))
  fit_nhpp(DataCPSim, chromo2tau(mat_cp[1,])) |>
    BMDL()
  
  # log-posterior
  fit_nhpp(DataCPSim, tau)
  
  penalty_mdl(pad_tau(tau, n = length(DataCPSim)))
  penalization_MDL(mat_cp[1,], "W", N = length(exceedances(DataCPSim)))
  
  expect_equal(penalty_mdl(pad_tau(0, n = 1)), -Inf)
  expect_equal(penalty_mdl(pad_tau(50, n = 100)), 3 * log(50))
  expect_gt(penalty_mdl(pad_tau(c(25, 50), n = 100)), penalty_mdl(pad_tau(50, n = 100)))
  
  x <- test_set()
  cpt <- attr(x, "cpt_true")
  expect_gt(penalty_mdl(pad_tau(cpt, length(x))), 0)
  
  # expect_equal(bmdl(x, 0), -Inf)
  true_bmdl <- fit_nhpp(x, cpt) |> BMDL()
  expect_type(true_bmdl, "double")
})
