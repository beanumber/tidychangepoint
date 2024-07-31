#' Fit a model for mean and variance
#' @inheritParams fit_meanshift
#' @param ... currently ignored
#' @export
#' @details
#' In a mean-variance model, both the means and variances are allowed to vary
#' across regions. 
#' Thus, this model fits a separate \eqn{\mu_j} and \eqn{\sigma_j} for each 
#' region \eqn{j}.
#' 
#' 
#' @returns A [mod_cpt] object.
#' @family model-fitting
#' @seealso [changepoint::cpt.meanvar()]
#' @examples
#' # Fit a mean-variance model
#' fit_meanvar(CET, tau = c(42, 330))
#' 
fit_meanvar <- function(x, tau, ...) { 
  if (!is_valid_tau(tau, length(x))) {
    stop("Invalid changepoint set")
  } else {
    tau <- unique(tau)
  }
  regions <- x |> 
    as.ts() |>
    split_by_tau(tau)
  
  region_mods <- regions |>
    purrr::map(~fit_meanshift_norm(.x, tau = NULL))
  
  fitted_values <- region_mods |>
    purrr::map(~c(fitted(.x))) |>
    purrr::list_c()
  
  region_params <- region_mods |>
    purrr::map(purrr::pluck("region_params")) |>
    purrr::list_rbind() |>
    dplyr::mutate(region = names(regions))
  
  region_params$param_sigma_hatsq <- region_mods |>
    purrr::map_dbl(model_variance)
  
  mod_cpt(
    x <- as.ts(x),
    tau = tau,
    region_params = region_params,
    model_params = c(),
    fitted_values = fitted_values,
    model_name = "meanvar"
  )
}

# Register model-fitting functions
fit_meanvar <- fun_cpt("fit_meanvar")
