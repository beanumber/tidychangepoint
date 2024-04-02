test_that("data works", {
  expect_s3_class(bogota_pm, "xts")
})

test_that("tidycpt works", {
  x <- segment(DataCPSim, method = "pelt")
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
  pelt_bic <- segment(DataCPSim, method = "pelt", penalty = "BIC")
  pelt_bic |>
    glance() |> 
    dplyr::select(dplyr::matches("IC|Lik"))
  
  z <- segment(DataCPSim, method = "manual", cpts = c(365, 826))
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
  
  expect_s3_class(segment(bogota_pm, method = "manual", cpts = c(500, 850)), "tidycpt")
  expect_error(segment(bogota_pm, method = "manual", tau = c(500, 850)), "cpts")
  
})

test_that("regions works", {
  x <- segment(DataCPSim, method = "pelt")
  tau <- changepoints(x)
  expect_equal(tau, changepoints(x$segmenter))
  expect_equal(tau, changepoints(x$nhpp))
  expect_false(0 %in% tau)
  expect_false(length(x) %in% tau)
  y <- split_by_tau(as.ts(x), tau)
  expect_equal(length(y), length(tau) + 1)
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
  expect_equal(y, pad_tau(c(826, 283764), length(x)))
  
  expect_false(is_valid_tau(0, length(x)))
  expect_true(is_valid_tau(1, length(x)))
  expect_true(is_valid_tau(826, length(x)))
  expect_true(is_valid_tau(length(x), length(x)))
  expect_false(is_valid_tau(length(x) + 1, length(x)))
  
  z <- cut_inclusive(x, y)
  expect_equal(length(z), length(x))
  expect_type(levels(z), "character")
  expect_length(levels(z), 2)
  
  z <- cut_inclusive(1:length(x), y)
  expect_equal(length(z), length(x))
  expect_type(levels(z), "character")
  expect_length(levels(z), 2)
  
  r <- random_cpts(DataCPSim, n = 5)
  expect_type(r, "list")
  expect_true(all(purrr::map(r, typeof) == "integer"))
})

test_that("penalties work", {
  library(tidychangepoint)
  mat_cp <- sim_k_cp_BMDL(DataCPSim)
  
  mat_cp |>
    mat_cp_2_list() |>
    purrr::map(fit_nhpp, x = DataCPSim) |>
    purrr::map_dbl(BMDL)

  tau <- chromo2tau(mat_cp[1,])
  b <- fit_nhpp(DataCPSim, chromo2tau(mat_cp[1,])) |>
    BMDL()
  
  # log-posterior
  fit_nhpp(DataCPSim, tau)
  
  mod <- fit_nhpp(DataCPSim, tau = NULL)
  expect_equal(MDL(mod), as.numeric(-2 * logLik(mod)))
  
  mod <- fit_nhpp(DataCPSim, tau = 365)
  expect_gt(MDL(mod), as.numeric(-2 * logLik(mod)))
  
  x <- test_set()
  cpt <- attr(x, "cpt_true")
  mod <- fit_nhpp(x, tau = cpt)
  expect_gt(MDL(mod), as.numeric(-2 * logLik(mod)))
  
  # expect_equal(bmdl(x, 0), -Inf)
  true_bmdl <- fit_nhpp(x, cpt) |> BMDL()
  expect_type(true_bmdl, "double")
})


test_that("performance comparison works", {
  x <- segment(DataCPSim, method = "pelt")
  y <- segment(DataCPSim, method = "gbmdl", num_generations = 20)
  z <- segment(DataCPSim, method = "random", num_generations = 20)
  expect_gt(BMDL(x), BMDL(y))
  expect_gt(BMDL(z), BMDL(y))
  
  expect_s3_class(dplyr::bind_rows(glance(x), glance(y), glance(z)), "tbl_df")
})
