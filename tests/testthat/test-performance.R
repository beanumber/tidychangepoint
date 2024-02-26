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
    )
  readr::write_rds(test_sets, file = here::here("tests/testthat/test_sets.rda"))
})

test_that("algs works", {
  skip()
  test_sets <- readr::read_rds(here::here("tests/testthat/test_sets.rda"))
  test_sets <- test_sets |>
    dplyr::mutate(
      bmdl_pelt = purrr::map_dbl(pelt, BMDL),
      genetic = purrr::map(data, segment, method = "gbmdl", num_generations = 10),
      bmdl_gbmdl = purrr::map_dbl(genetic, BMDL)
    )
  readr::write_rds(test_sets, file = here::here("tests/testthat/test_sets.rda"))
})
  
test_that("performance works", {
  skip()
  test_sets <- readr::read_rds(here::here("tests/testthat/test_sets.rda"))
  test_long <- test_sets |>
    dplyr::select(ncpts_true, contains("bmdl")) |>
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
  test_long <- test_sets |>
    dplyr::mutate(
      pelt_time = purrr::map_dbl(pelt, purrr::pluck, "elapsed_time"),
      genetic_time = purrr::map_dbl(genetic, purrr::pluck, "elapsed_time")
    ) |>
    dplyr::select(ncpts_true, contains("time")) |>
    tidyr::pivot_longer(cols = -ncpts_true, names_to = "algorithm", values_to = "time")
  
  ggplot2::ggplot(test_long, ggplot2::aes(x = ncpts_true, y = time, color = algorithm)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = 0) + 
    ggplot2::scale_x_continuous("True number of changepoints") +
    ggplot2::scale_y_continuous("Elapsed time (seconds)") +
    ggplot2::labs(
      title = "Comparison of running time across algorithms",
      subtitle = paste(nrow(test_sets), "test data sets")
    )
})

