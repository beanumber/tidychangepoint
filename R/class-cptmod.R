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
#' cpt <- cptmod(CET)
#' str(cpt)
#' as.ts(cpt)
#' changepoints(cpt)

new_cptmod <- function(x = numeric(), 
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
    class = "cptmod"
  )
}

#' @rdname new_cptmod
#' @export

validate_cptmod <- function(x) {
  if (!stats::is.ts(as.ts(x))) {
    stop("data attribute is not coercible into a ts object.")
  }
  x
}

#' @rdname new_cptmod
#' @export

cptmod <- function(x, ...) {
  obj <- new_cptmod(x, ...)
  validate_cptmod(obj)
}

#' Methods for cptmod objects
#' @name cptmod-generics
#' @param x A `cptmod` object, typically the output from one of the `fit_*()`
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
#' glance(cpts)

as.ts.cptmod <- function(x, ...) {
  as.ts(x$data)
}

#' @rdname cptmod-generics
#' @export
nobs.cptmod <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname cptmod-generics
#' @inheritParams stats::logLik
#' @export
logLik.cptmod <- function(object, ...) {
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

#' @rdname cptmod-generics
#' @export
fitted.cptmod <- function(object, ...) {
  object$fitted_values
}

#' @rdname cptmod-generics
#' @export
residuals.cptmod <- function(object, ...) {
  object$data - fitted(object)
}

#' @rdname cptmod-generics
#' @export
model_variance <- function(object, ...) {
  sum(residuals(object)^2) / nobs(object)
}

#' @rdname changepoints
#' @export
changepoints.cptmod <- function(x, ...) {
  x$tau |>
    as.integer()
}

#' @rdname cptmod-generics
#' @export
glance.cptmod <- function(x, ...) {
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


