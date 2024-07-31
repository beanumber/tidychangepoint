# Unused functions in development

fit_meanshift2 <- function(x, tau, ...) {
  y <- as.numeric(as.ts(x))
  N <- length(y) # length of the series
  m <- length(tau) # Number of CPTs
  
  regions <- y |>
    split_by_tau(tau) |>
    purrr::map(fit_meanshift_region, ...)
  
  region_names <- names(regions)
  
  region_params <- regions |>
    purrr::map(1) |>
    purrr::list_rbind() |>
    dplyr::mutate(region = region_names, .before = dplyr::everything())
  
  y_hat <- regions |>
    purrr::map(2) |>
    purrr::list_c()
  
  out <- mod_cpt(
    x = y,
    tau = tau,
    region_params = region_params,
    model_params = c(
      sigma_hatsq = sum((y - y_hat)^2) / N
    ),
    fitted_values = y_hat,
    model_name = "meanshift"
  )
  return(out)
}

fit_meanshift_region <- function(x, ...) {
  y <- as.numeric(x)
  N <- length(y)
  mu <- mean(y, na.rm = TRUE)
  list(
    region_params = tibble::tibble(
      param_mu = mu
    ),
    fitted_values = rep(mu, N)
  )
}
