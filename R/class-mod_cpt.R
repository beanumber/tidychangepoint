globalVariables(c("bmdl", "nhpp", "cpt_length", "value", ".fitted", ".resid"
                  , "xintercept"))

#' @rdname mod_cpt
#' @export
new_mod_cpt <- function(x = numeric(), 
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
    class = "mod_cpt"
  )
}

#' @rdname mod_cpt
#' @export
validate_mod_cpt <- function(x) {
  if (!stats::is.ts(as.ts(x))) {
    stop("data attribute is not coercible into a ts object.")
  }
  if (!is_valid_tau(x$tau, nobs(x))) {
#    message("Removing 1 and/or n from tau...")
    x$tau <- validate_tau(x$tau, nobs(x))
  }
  x
}

#' Base class for changepoint models
#' @description
#' Create changepoint detection model objects
#' @details
#' Changepoint detection models know how they were created, on what data set,
#' about the optimal changepoint set found, and the parameters that were fit
#' to the model. 
#' Methods for various generic reporting functions are provided. 
#' 
#' All changepoint detection models inherit from [mod_cpt]: the 
#' base class for changepoint detection models. 
#' These models are created by one of the `fit_*()` functions, or by 
#' [as.model()]. 
#' 
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
#' @param model_name A `character` vector giving the model's name. 
#' @param ... currently ignored
#' @returns A [mod_cpt] object
#' @seealso [as.model()]
#' @examples
#' cpt <- mod_cpt(CET)
#' str(cpt)
#' as.ts(cpt)
#' changepoints(cpt)
mod_cpt <- function(x, ...) {
  obj <- new_mod_cpt(x, ...)
  validate_mod_cpt(obj)
}

#' @rdname reexports
#' @export
as.ts.mod_cpt <- function(x, ...) {
  as.ts(x$data)
}

#' @rdname reexports
#' @export
nobs.mod_cpt <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname reexports
#' @export
logLik.mod_cpt <- function(object, ...) {
  sigma_hatsq <- model_variance(object)
  if ("durbin_watson" %in% names(object)) {
    N <- nobs(object) - 1
  } else {
    N <- nobs(object)
  }
  ll <- -N * (log(sigma_hatsq) + 1 + log(2 * pi)) / 2
  as.logLik(object, ll)
}

as.logLik <- function(object, ll = 0) {
  m <- length(object$tau)
  num_params_per_region <- object |>
    coef() |>
    dplyr::select(dplyr::contains("param_")) |>
    ncol()
  num_model_params <- length(object$model_params)
  
  if (!is.numeric(ll)) {
    warning("Invalid log-likelihood value...returning 0")
    ll <- 0
  }
  attr(ll, "num_params_per_region") <- num_params_per_region
  attr(ll, "num_model_params") <- num_model_params
  attr(ll, "df") <- m + num_params_per_region * (m + 1) + num_model_params
  attr(ll, "nobs") <- nobs(object)
  attr(ll, "tau") <- object$tau
  class(ll) <- "logLik"
  return(ll)
}

#' @rdname reexports
#' @export
fitted.mod_cpt <- function(object, ...) {
  object$fitted_values
}

#' @rdname reexports
#' @export
residuals.mod_cpt <- function(object, ...) {
  object$data - fitted(object)
}

#' Compute model variance
#' @param object A model object implementing [residuals()] and [nobs()]
#' @param ... currently ignored
#' @details
#' Using the generic functions [residuals()] and [nobs()], this function 
#' computes the variance of the residuals. 
#' 
#' Note that unlike [stats::var()], it does not use \eqn{n-1} as the denominator.
#' @returns A `double` vector of length 1
#' @export
model_variance <- function(object, ...) {
  sum(residuals(object)^2) / nobs(object)
}

#' @rdname reexports
#' @export
coef.mod_cpt <- function(object, ...) {
  object$region_params
}

#' @rdname model_name
#' @export
model_name.mod_cpt <- function(object, ...) {
  object$model_name
}


#' @rdname changepoints
#' @export
changepoints.mod_cpt <- function(x, ...) {
  x$tau |>
    as.integer()
}

#' @rdname reexports
#' @export
augment.mod_cpt <- function(x, ...) {
  tau <- changepoints(x)
  tibble::enframe(as.ts(x), name = "index", value = "y") |>
    dplyr::mutate(index = as.integer(index)) |>
    tsibble::as_tsibble(index = index) |>
    dplyr::mutate(
      region = cut_by_tau(index, pad_tau(tau, nobs(x))),
      .fitted = fitted(x),
      .resid = residuals(x)
    ) |>
    dplyr::group_by(region)
}

#' @rdname reexports
#' @export
tidy.mod_cpt <- function(x, ...) {
  tau <- changepoints(x)
  n <- nobs(x)
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
    dplyr::inner_join(coef(x), by = "region")
}

#' @rdname reexports
#' @export
glance.mod_cpt <- function(x, ...) {
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
  
  phi_hat <- sum(resid[-1] * resid[-n] ) / sum(resid^2)
  d <- sum((resid[-n] - resid[-1])^2) / sum(resid^2)
  y_hat <- fitted(mod) + c(0, phi_hat * resid[-n])
  sigma_hatsq <- sum((y - y_hat)^2) / n
  
  out <- mod
  out$fitted_values <- y_hat
  out$model_params[["sigma_hatsq"]] <- sigma_hatsq
  out$model_params[["phi_hat"]] <- phi_hat
  out$durbin_watson <- d
  out$model_name <- paste0(out$model_name, "_ar1")
  return(out)
}


#' @rdname reexports
#' @export
#' @examples
#' # Plot a meanshift model fit
#' plot(fit_meanshift_norm(CET, tau = 330))
#' plot(fit_meanshift_norm(CET, tau = 330), plot.title.position = "plot")
#' 
#' #' # Plot a trendshift model fit
#' plot(fit_trendshift(CET, tau = 330))
#' 
#' #' # Plot a quadratic polynomial model fit
#' plot(fit_lmshift(CET, tau = 330, deg_poly = 2))
#' 
#' #' # Plot a 4th degree polynomial model fit
#' plot(fit_lmshift(CET, tau = 330, deg_poly = 10))
#' 
plot.mod_cpt <- function(x, ...) {
  dots <- list(...)
  regions <- tidy(x)
  m <- length(changepoints(x))
  if (m > 2 && is.null(dots[["axis.text.x"]])) {
    dots[["axis.text.x"]] <- ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)
  }
  breaks_default <- scales::extended_breaks(3)(1:nobs(x))
  b <- c(1, changepoints(x), nobs(x))
  if (m == 0) {
    b2 <- c(1, breaks_default, nobs(x)) |>
      sort()
    b <- b2[b2 > 0]
  }
  ggplot2::ggplot(
    data = augment(x), 
    ggplot2::aes(x = index, y = y, group = region)
  ) +
    #    ggplot2::geom_rect(
    #      data = regions,
    #      ggplot2::aes(xmin = begin, xmax = end, ymin = 0, ymax = Inf, x = NULL, y = NULL),
    #      fill = "grey90"
    #    ) +
    ggplot2::geom_vline(
      data = tibble::tibble(xintercept = unique(c(1, changepoints(x), nobs(x)))),
      ggplot2::aes(xintercept = xintercept), 
      linetype = 2
    ) +
    ggplot2::geom_hline(yintercept = mean(as.ts(x)), linetype = 3) +
    ggplot2::geom_rug(sides = "l") +
    ggplot2::geom_line() + 
    ggplot2::geom_line(ggplot2::aes(y = .fitted), color = "red") + 
#    ggplot2::geom_segment(
#      data = regions,
#      ggplot2::aes(x = begin, y = mean, xend = end, yend = mean),
#      color = "red"
#    ) +
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
    ggplot2::scale_x_continuous("Time Index (t)", breaks = b) +
    ggplot2::labs(
#      title = "Original time series",
      subtitle = paste("Global mean value is", round(mean(as.ts(x), na.rm = TRUE), 2))
    ) +
    do.call(ggplot2::theme, dots)
}


#' @rdname reexports
#' @export
print.mod_cpt <- function(x, ...) {
  r_params <- ncol(dplyr::select(coef(x), dplyr::contains("param_")))
  cli::cli_alert_info(
    paste("Model: A {.emph", model_name(x), "} model with", length(regions(x)), "region(s).")
  )
  cli::cli_alert(paste("Each region has", r_params, "parameter(s)."))
  cli::cli_alert(paste("The model has", length(x$model_params), "global parameter(s)."))
}

#' @rdname reexports
#' @export
summary.mod_cpt <- function(object, ...) {
  cli::cli_alert_info("Model")
  cli::cli_alert(
    paste("M: Fit the {.emph", model_name(object), "} model.")
  )
  cli::cli_alert(
    paste(
      "\u03b8: Estimated", ncol(dplyr::select(coef(object), dplyr::contains("param_"))), 
      "parameter(s), for each of", nrow(coef(object)), "region(s)."
    )
  )
}

#' @rdname regions
#' @export
#' @examples
#' 
#' cpt <- fit_meanshift_norm(CET, tau = 330)
#' regions(cpt)
#' 
regions.mod_cpt <- function(x, ...) {
  regions_tau(changepoints(x), nobs(x))
}

#' @rdname diagnose
#' @export
#' @examples
#' # For meanshift models, show the distribution of the residuals by region
#' fit_meanshift_norm(CET, tau = 330) |>
#' diagnose()
diagnose.mod_cpt <- function(x, ...) {
  ggplot2::ggplot(
    data = augment(x), 
    ggplot2::aes(x = region, y = .resid)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = 3) +
    ggplot2::geom_violin(alpha = 0.5, ggplot2::aes(fill = region)) +
    ggplot2::geom_rug(sides = "l") +
    ggplot2::scale_x_discrete("Region defined by changepoint set") +
    ggplot2::scale_y_continuous("Residual") + 
    ggplot2::labs(
      title = "Distribution of residuals by region"
    )
}
