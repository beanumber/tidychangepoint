globalVariables(c("bmdl", "nhpp", "cpt_length", "value"))

#' Base class for changepoint models
#' @export
#' @param x a numeric vector coercible into a `ts` object
#' @param tau indices of the changepoint set
#' @param region_params A [tibble::tibble()] with one row for each region 
#' defined by the changepoint set `tau`. Each variable represents a parameter
#' estimated in that region. 
#' @param model_params A numeric vector of parameters estimated by the model
#' across the entire data set (not just in each region). 
#' @param fitted_values Fitted values returned by the model on the original
#' data set. 
#' @param ... currently ignored
#' @examples
#' cpt <- mod_default(CET)
#' str(cpt)
#' as.ts(cpt)
#' changepoints(cpt)

new_mod_default <- function(x = numeric(), 
                       tau = integer(),
                       region_params = tibble::tibble(),
                       model_params = double(),
                       fitted_values = double(), 
                       model_name = character(), ...) {
  stopifnot(is.numeric(x))
  structure(
    list(
      data = stats::as.ts(x),
      tau = tau,
      region_params = region_params,
      model_params = model_params,
      fitted_values = fitted_values,
      model_name = model_name
    ), 
    class = "mod_default"
  )
}

#' @rdname new_mod_default
#' @export

validate_mod_default <- function(x) {
  if (!stats::is.ts(as.ts(x))) {
    stop("data attribute is not coercible into a ts object.")
  }
  if (!is_valid_tau(x$tau, nobs(x))) {
#    message("Removing 1 and/or n from tau...")
    x$tau <- x$tau[x$tau %in% 2:(nobs(x) - 1)]
  }
  x
}

#' @rdname new_mod_default
#' @export

mod_default <- function(x, ...) {
  obj <- new_mod_default(x, ...)
  validate_mod_default(obj)
}

#' Methods for mod_default objects
#' @name mod_default-generics
#' @param x A `mod_default` object, typically the output from one of the `fit_*()`
#' functions
#' @export
#' @examples
#' cpts <- fit_meanshift(DataCPSim, tau = 365)
#' as.ts(cpts)
#' nobs(cpts)
#' logLik(cpts)
#' fitted(cpts)
#' residuals(cpts)
#' changepoints(cpts)
#' augment(cpts)
#' tidy(cpts)
#' glance(cpts)

as.ts.mod_default <- function(x, ...) {
  as.ts(x$data)
}

#' @rdname mod_default-generics
#' @export
nobs.mod_default <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname mod_default-generics
#' @inheritParams stats::logLik
#' @export
logLik.mod_default <- function(object, ...) {
  m <- length(object$tau)
  N <- nobs(object)
  sigma_hatsq <- model_variance(object)
  ll <- -(N * log(sigma_hatsq) + N + N * log(2 * pi)) / 2
  num_region_params <- object$region_params |>
    dim() |>
    prod()
  num_model_params <- length(object$model_params)
  attr(ll, "num_region_params") <- num_region_params
  attr(ll, "num_model_params") <- num_model_params
  attr(ll, "df") <- m + num_region_params + num_model_params
  attr(ll, "nobs") <- N
  attr(ll, "tau") <- object$tau
  class(ll) <- "logLik"
  return(ll)
}

#' @rdname mod_default-generics
#' @export
fitted.mod_default <- function(object, ...) {
  object$fitted_values
}

#' @rdname mod_default-generics
#' @export
residuals.mod_default <- function(object, ...) {
  object$data - fitted(object)
}

#' @rdname mod_default-generics
#' @export
model_variance <- function(object, ...) {
  sum(residuals(object)^2) / nobs(object)
}

#' @rdname changepoints
#' @export
changepoints.mod_default <- function(x, ...) {
  x$tau |>
    as.integer()
}

#' @rdname mod_default-generics
#' @seealso [broom::augment()]
#' @export
augment.mod_default <- function(x, ...) {
  tau <- changepoints(x)
  tibble::enframe(as.ts(x), name = "index", value = "y") |>
    tsibble::as_tsibble(index = index) |>
    dplyr::mutate(
      region = cut_inclusive(index, pad_tau(tau, nobs(x)))
    ) |>
    dplyr::group_by(region)
}

#' @rdname mod_default-generics
#' @seealso [broom::tidy()]
#' @export
tidy.mod_default <- function(x, ...) {
  tau <- changepoints(x)
  n <- length(as.ts(x))
  tau_padded <- pad_tau(tau, n)
  
  augment(x) |>
    dplyr::ungroup() |>
    # why is this necessary????
    as.data.frame() |>
    dplyr::group_by(region) |>
    dplyr::summarize(
      num_obs = dplyr::n(),
#      begin = min(y),
#      end = max(y),
      min = min(y, na.rm = TRUE),
      max = max(y, na.rm = TRUE),
      mean = mean(y, na.rm = TRUE),
      sd = stats::sd(y, na.rm = TRUE),
#      ... = ...
    ) |>
    dplyr::mutate(
      begin = utils::head(tau_padded, -1),
      end = utils::tail(tau_padded, -1)
    ) |>
    dplyr::inner_join(x$region_params, by = "region")
}

#' @rdname mod_default-generics
#' @export
glance.mod_default <- function(x, ...) {
  tibble::tibble(
    pkg = "tidychangepoint",
    version = package_version(utils::packageVersion("tidychangepoint")),
    algorithm = x$model_name,
    params = list(x$model_params),
    num_cpts = length(changepoints(x)),
    rmse = sqrt(model_variance(x)),
    logLik = as.double(logLik(x)),
    AIC = AIC(x),
    BIC = BIC(x),
    MBIC = MBIC(x),
    MDL = MDL(x)
  )
}

autoregress_errors <- function(mod, ...) {
  n <- nobs(mod)
  resid <- residuals(mod)
  
  y <- as.ts(mod)
  
  phi_hat <- sum(utils::head(resid, -1) * utils::tail(resid, -1)) / sum(resid^2)
  d <- sum((utils::head(resid, -1) - utils::tail(resid, -1))^2) / sum(resid^2)
  y_hat <- fitted(mod) + c(0, phi_hat * utils::head(resid, -1))
  sigma_hatsq <- sum((y - y_hat)^2) / n
  
  out <- mod
  out$fitted_values <- y_hat
  out$model_params[["sigma_hatsq"]] <- sigma_hatsq
  out$model_params[["phi_hat"]] <- phi_hat
  out$durbin_watson <- d
  out$model_name <- paste0(out$model_name, "_ar1")
  return(out)
}


#' @rdname mod_default-generics
#' @param use_time_index Should the x-axis labels be the time indices? Or the 
#' time labels? 
#' @export
#' @examples
#' plot(fit_meanshift(CET, tau = 300))
plot.mod_default <- function(x, ...) {
  regions <- tidy(x)
  ggplot2::ggplot(
    data = augment(x), 
    ggplot2::aes(x = index, y = y)
  ) +
    #    ggplot2::geom_rect(
    #      data = regions,
    #      ggplot2::aes(xmin = begin, xmax = end, ymin = 0, ymax = Inf, x = NULL, y = NULL),
    #      fill = "grey90"
    #    ) +
    ggplot2::geom_vline(data = regions, ggplot2::aes(xintercept = end), linetype = 3) +
    ggplot2::geom_hline(yintercept = mean(as.ts(x)), linetype = 3) +
    ggplot2::geom_rug(sides = "l") +
    ggplot2::geom_line() + 
    ggplot2::geom_segment(
      data = regions,
      ggplot2::aes(x = begin, y = mean, xend = end, yend = mean),
      color = "red"
    ) +
    ggplot2::geom_segment(
      data = regions,
      ggplot2::aes(x = begin, y = mean + 1.96 * sd, xend = end, yend = mean + 1.96 * sd),
      color = "red",
      linetype = 3
    ) +
    ggplot2::geom_segment(
      data = regions,
      ggplot2::aes(x = begin, y = mean - 1.96 * sd, xend = end, yend = mean - 1.96 * sd),
      color = "red",
      linetype = 3
    ) + 
    ggplot2::scale_x_continuous("Time Index (t)") +
    ggplot2::scale_y_continuous("Original Measurement") + 
    ggplot2::labs(
      title = "Original times series",
      subtitle = paste("Mean value is", round(mean(as.ts(x), na.rm = TRUE), 2))
    )
}
