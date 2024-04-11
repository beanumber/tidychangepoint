#' Fit a model for mean and variance
#' @inheritParams fit_meanshift
#' @export
#' @examples
#' fit_meanvar(CET, tau = c(42, 330))

fit_meanvar <- function(x, tau, ...) {
  regions <- x |> 
    as.ts() |>
    split_by_tau(tau)
  
  region_mods <- regions |>
    purrr::map(~fit_lmshift(.x, tau = NULL))
  
  fitted_values <- region_mods |>
    purrr::map(~c(fitted(.x))) |>
    purrr::list_c()
  
  region_params <- region_mods |>
    purrr::map(purrr::pluck("region_params")) |>
    purrr::list_rbind() |>
    dplyr::mutate(region = names(regions))
  
  region_params$sigma_hatsq <- region_mods |>
    purrr::map_dbl(model_variance)
  
  mod_default(
    x <- as.ts(x),
    tau = tau,
    region_params = region_params,
    model_params = c(),
    fitted_values = fitted_values,
    model_name = "meanvar"
  )
}

attr(fit_meanvar, "model_name") <- "meanvar"
