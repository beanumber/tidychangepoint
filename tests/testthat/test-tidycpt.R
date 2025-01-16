test_that("data works", {
  expect_s3_class(bogota_pm, "xts")
})

test_that("tidycpt works", {
  x <- segment(DataCPSim, method = "pelt")
  expect_s3_class(x, "tidycpt")
  expect_s3_class(as.ts(x), "ts")
  expect_s3_class(augment(x), "grouped_ts")
  expect_s3_class(tidy(x), "tbl_df")
  expect_s3_class(glance(x), "tbl_df")
  expect_type(changepoints(x), "integer")
  expect_s3_class(plot(x), "gg")
  expect_s3_class(diagnose(x), "patchwork")
  
  z <- segment(DataCPSim, method = "manual", tau = c(365, 826))
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
  
  expect_s3_class(segment(bogota_pm, method = "manual", tau = c(500, 850)), "tidycpt")
  expect_error(segment(bogota_pm, method = "manual", cpts = c(500, 850)), "tau")
  
})

test_that("regions works", {
  x <- segment(DataCPSim, method = "pelt")
  tau <- changepoints(x)
  expect_equal(tau, changepoints(x$segmenter))
  expect_equal(tau, changepoints(x$model))
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
  expect_true(1 %in% y)
  expect_true(all(tau %in% y))
  expect_true((length(x) + 1) %in% y)
  expect_equal(unpad_tau(y), tau)
  expect_false(0 %in% unpad_tau(y))
  expect_false(1 %in% unpad_tau(y))
  expect_true(all(tau %in% y))
  expect_false(length(x) %in% unpad_tau(y))
  expect_false((length(x) + 1) %in% unpad_tau(y))
  expect_equal(y, pad_tau(c(826, 283764), length(x)))
  
  expect_false(is_valid_tau(0, length(x)))
  expect_false(is_valid_tau(1, length(x)))
  expect_true(is_valid_tau(826, length(x)))
  expect_false(is_valid_tau(length(x), length(x)))
  expect_false(is_valid_tau(length(x) + 1, length(x)))
  
  expect_length(validate_tau(0, length(x)), 0)
  expect_length(validate_tau(1, length(x)), 0)
  expect_length(validate_tau(826, length(x)), 1)
  expect_length(validate_tau(length(x), length(x)), 0)
  expect_length(validate_tau(length(x) + 1, length(x)), 0)
  expect_length(validate_tau(c(826, 826), length(x)), 1)
  expect_length(validate_tau(c(-4, 0, 1, 4, 5, 5, 824, 1096, 1097, 182384), length(x)), 3)
  
  w <- regions_tau(tau, length(x))
  expect_length(w, 2)
  expect_s3_class(w, "factor")
  expect_match(levels(w), "^\\[.+\\,.+\\)$")
  
  z <- cut_by_tau(x, y)
  expect_equal(length(z), length(x))
  expect_type(levels(z), "character")
  expect_length(levels(z), 2)
  expect_match(levels(z), "^\\[.+\\,.+\\)$")
  
  z <- cut_by_tau(1:length(x), y)
  expect_equal(length(z), length(x))
  expect_type(levels(z), "character")
  expect_length(levels(z), 2)
  
  ds <- data.frame(y = as.ts(CET), t = 1:length(CET))
  x <- tbl_coef(lm(y ~ 1, data = ds))
  expect_s3_class(x, "tbl_df")
  expect_equal(ncol(x), 2)
  expect_identical(names(x), c("region", "param_mu"))
  y <- tbl_coef(lm(y ~ (t >= 42) + (t >= 81), data = ds))
  expect_s3_class(y, "tbl_df")
  expect_equal(ncol(y), 2)
  expect_identical(names(y), c("region", "param_mu"))
  z <- tbl_coef(lm(y ~ poly(t, 1, raw = TRUE) * (t >= 42) + poly(t, 1, raw = TRUE) * (t >= 81), data = ds))
  expect_s3_class(z, "tbl_df")
  expect_equal(ncol(z), 3)
  expect_identical(names(z), c("region", "param_mu", "param_beta1"))
  w <- tbl_coef(lm(y ~ poly(t, 2, raw = TRUE) * (t >= 42) + poly(t, 2, raw = TRUE) * (t >= 81), data = ds))
  expect_s3_class(w, "tbl_df")
  expect_equal(ncol(w), 4)
  expect_identical(names(w), c("region", "param_mu", "param_beta1", "param_beta2"))
    
  expect_equal(model_name(fit_meanshift_norm), "meanshift_norm")
  expect_equal(model_name(fit_meanshift_norm_ar1), "meanshift_norm_ar1")
  expect_equal(model_name(fit_lmshift), "lmshift")
  expect_equal(model_name(fit_nhpp), "nhpp")
  
  x <- fit_meanshift_norm(CET, tau = 42)
  expect_s3_class(whomademe(x), "fun_cpt")
  expect_equal(model_name(whomademe(x)), model_name(x))
})

test_that("penalties work", {
  library(tidychangepoint)
  mat_cp <- sim_k_cp_BMDL(DataCPSim)
  
  mat_cp |>
    mat_cp_2_list() |>
    purrr::map(fit_nhpp, x = DataCPSim) |>
    purrr::map_dbl(BMDL)

  tau <- chromo2tau(mat_cp[1,])
  b <- fit_nhpp(DataCPSim, chromo2tau(mat_cp[1,]))
  expect_lt(MDL(b), BMDL(b))
  
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
  
  true_bmdl <- fit_nhpp(x, cpt) |> BMDL()
  expect_type(true_bmdl, "double")
})


test_that("modeling works", {
  expect_error(fit_meanshift_norm(CET, tau = 0))
  expect_error(fit_meanshift_norm(CET, tau = 1))
  expect_error(fit_meanshift_norm(CET, tau = length(CET)))
  expect_equal(
    fit_meanshift_norm(CET, tau = c(42, 42)) |>
      changepoints() |>
      length(),
    1
  )
  expect_error(segment(CET, method = "manual", tau = 0))
  expect_error(segment(CET, method = "manual", tau = 1))
  expect_error(segment(CET, method = "manual", tau = length(CET)))
  
#  x <- segment(CET, method = "ga-coen", maxiter = 5)
#  x <- segment(mlb_hrs, method = "ga-shi", maxiter = 50)
})
