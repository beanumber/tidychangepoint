test_that("performance comparison works", {
  skip()
  x <- segment(DataCPSim, method = "pelt")
  #  y <- segment(DataCPSim, method = "coen", num_generations = 20)
  y <- segment(DataCPSim, method = "ga-coen", maxiter = 20)
  z <- segment(DataCPSim, method = "random", model_fn = fit_nhpp, penalty_fn = BMDL, popSize = 20)
  
  expect_gt(BMDL(fit_nhpp(DataCPSim, changepoints(x))), BMDL(y$model))
  expect_gt(BMDL(z$model), BMDL(y$model))
  
  expect_s3_class(dplyr::bind_rows(glance(x), glance(y), glance(z)), "tbl_df")
})


test_that("random performance", {
  skip()
  x <- segment(CET, method = "random", popSize = 100)
  y <- segment(CET, method = "ga-random", popSize = 100)
  bench::mark(
    basket = segment(CET, method = "random", popSize = 100),
    ga = segment(CET, method = "ga-random", popSize = 100),
    check = FALSE
  )
})

test_that("ga performance", {
  skip()
  segment_ga_shi(CET, maxiter = 50)
  # slow
  loc_ind <- round(runif(length(CET)))
  tau <- binary2tau(loc_ind)
  
  BIC(mod <- fit_lmshift(CET, tau = tau, ar1 = TRUE))
  BIC(mod2 <- fit_meanshift_ar1(CET, loc.ind = loc_ind))
  
  bench::mark(
    "lm" = BIC(fit_lmshift(CET, tau = tau, ar1 = TRUE)),
    "shi" = BIC(fit_meanshift_ar1(CET, loc.ind = loc_ind))
  )

})
    
test_that("test_sets works", {
  skip()
  test_sets <- rep(1:12, 3) |>
    purrr::map(test_set) |>
    tibble::enframe(value = "data") |>
    dplyr::mutate(
      cpt_true = purrr::map(data, attr, which = "cpt_true"),
      ncpts_true = purrr::map_int(cpt_true, length),
      nhpp_true = purrr::map2(data, cpt_true, fit_nhpp),
      bmdl_true = purrr::map_dbl(nhpp_true, BMDL),
      pelt = purrr::map(data, segment, method = "pelt"),
      bmdl_pelt = purrr::map_dbl(pelt, BMDL),
      is_pelt_true = identical(cpt_true, changepoints(pelt))
    )
  readr::write_rds(test_sets, file = here::here("tests/testthat/test_sets.rda"))
})

test_that("algs works", {
  skip()
  test_sets <- readr::read_rds(here::here("tests/testthat/test_sets.rda"))
  test_sets <- test_sets |>
    dplyr::mutate(
#      genetic = purrr::map(data, segment, method = "gbmdl", num_generations = 10),
      bmdl_gbmdl = purrr::map_dbl(genetic, BMDL), 
      is_gbmdl_true = bmdl_gbmdl == bmdl_true,
      is_gbmdl_better_than_pelt = bmdl_gbmdl < bmdl_pelt,
      is_gbmdl_better_than_true = bmdl_gbmdl < bmdl_true
    )
  readr::write_rds(test_sets, file = here::here("tests/testthat/test_sets.rda"))
})
  
test_that("performance works", {
  skip()
  test_sets <- readr::read_rds(here::here("tests/testthat/test_sets.rda"))
  test_sets |>
    dplyr::summarize(
      num_trials = dplyr::n(),
      pelt_true = sum(is_pelt_true) / dplyr::n(),
      gbmdl_true = sum(is_gbmdl_true) / dplyr::n(),
      gbmdl_better_pelt = sum(is_gbmdl_better_than_pelt) / dplyr::n(),
      gbmdl_better_true = sum(is_gbmdl_better_than_true)/ dplyr::n()
    )
  bad <- test_sets |>
    dplyr::filter(is_gbmdl_better_than_true) |>
    head(1)
  
  x <- bad$pelt[[1]]
  y <- bad$genetic[[1]]
  diagnose(x)
  diagnose(y)
  
  test_long <- test_sets |>
    dplyr::select(ncpts_true, dplyr::contains("bmdl")) |>
    tidyr::pivot_longer(cols = -ncpts_true, names_to = "algorithm", values_to = "bmdl")
  
  ggplot2::ggplot(test_long, ggplot2::aes(x = ncpts_true, y = bmdl, color = algorithm)) +
    ggplot2::geom_jitter(width = 0.1, alpha = 0.8) +
    ggplot2::geom_smooth(se = 0) + 
    ggplot2::scale_x_continuous("True number of changepoints") +
    ggplot2::scale_y_continuous("BMDL") +
    ggplot2::labs(
      title = "Comparison of BMDL scores across algorithms",
      subtitle = paste(nrow(test_sets), "test data sets")
    )
})

test_that("running time works", {
  skip()
  test_sets <- readr::read_rds(here::here("tests/testthat/test_sets.rda"))
  
  test_glance <- c(test_sets$pelt, test_sets$genetic) |>
    purrr::map(glance) |>
    dplyr::bind_rows()
  
  ggplot2::ggplot(test_glance, ggplot2::aes(x = num_cpts, y = elapsed_time, color = algorithm)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = 0) + 
    ggplot2::scale_x_continuous("True number of changepoints") +
    ggplot2::scale_y_continuous("Elapsed time (seconds)") +
    ggplot2::labs(
      title = "Comparison of running time across algorithms",
      subtitle = paste(nrow(test_sets), "test data sets")
    )
  
  test_glance |>
    dplyr::select(matches("algo|num_cpts|nhpp")) |>
    dplyr::mutate(nhpp_logLik = as.double(nhpp_logLik)) |>
    tidyr::pivot_longer(cols = -c(algorithm, num_cpts), names_to = "type", values_to = "value") |>
    ggplot2::ggplot(ggplot2::aes(x = num_cpts, y = value, color = type)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(se = 0) +
      ggplot2::facet_wrap(ggplot2::vars(algorithm))
  
})


test_that("random works", {
  skip()
  test_sets <- readr::read_rds(here::here("tests/testthat/test_sets.rda"))
  random_glance <- test_sets |>
    dplyr::mutate(
      random = purrr::map(data, segment, method = "random", num_generations = 100)
    ) |>
    dplyr::pull(random) |>
    purrr::map(glance) |>
    dplyr::bind_rows()
  
  random_glance |>
    dplyr::select(matches("algo|num_cpts|nhpp")) |>
    dplyr::mutate(nhpp_logLik = as.double(nhpp_logLik)) |>
    tidyr::pivot_longer(cols = -c(algorithm, num_cpts), names_to = "type", values_to = "value") |>
    ggplot2::ggplot(ggplot2::aes(x = num_cpts, y = value, color = type)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = 0) +
    ggplot2::facet_wrap(ggplot2::vars(algorithm))
})
