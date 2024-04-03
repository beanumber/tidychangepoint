#' Fast implementation of meanshift model
#' 
#' @inheritParams fit_lmshift
#' @export
#' @author Xueheng Shi
#' @examples
#' tau <- c(365, 826)
#' mod <- fit_meanshift_ar1(DataCPSim, tau)
#' logLik(mod)
#' deg_free(mod)
#' 
#' cpts <- c(1700, 1739, 1988)
#' ids <- time2tau(cpts, substr(time(CET), 1, 4))
#' mod <- fit_meanshift(CET, tau = ids)
#' glance(mod)
#' mod <- fit_meanshift_ar1(CET, tau = ids)
#' glance(mod)

fit_meanshift <- function(x, tau, ar1 = FALSE, ...) {
  y <- as.numeric(as.ts(x))
  N <- length(y) # length of the series
  m <- length(tau) # Number of CPTs
  
  y_seg <- y |>
    split_by_tau(tau)
  mu_seg <- y_seg |>
    purrr::map_dbl(mean)
  seg_len <- y_seg |>
    purrr::map_int(length)
  y_hat <- rep(mu_seg, seg_len)

  sigma_hatsq <- sum((y - y_hat)^2) / N
  out <- list(
    data = y,
    nobs = N,
    means = mu_seg,
    fitted.values = y_hat,
    residuals = y - y_hat,
    sigma_hatsq = sigma_hatsq,
    phi_hat = 0,
    tau = tau,
    model_name = "meanshift",
    ar1 = ar1,
    trends = FALSE
  )
  class(out) <- c("meanshift", class(out))
  
  if (ar1) {
    out <- autoregress_errors(out)
  }
  
  return(out)
}

#' @rdname fit_meanshift
#' @export
fit_meanshift_ar1 <- function(x, tau, ...) {
  fit_meanshift(x, tau, ar1 = TRUE, ...)
}

#' Methods for meanshift objects
#' @name meanshift-generics
#' @param x A `meanshift` object, typically the output from [fit_meanshift()]
#' @export
#' @examples
#' cpts <- fit_meanshift(DataCPSim, tau = 365)
#' as.ts(cpts)
as.ts.meanshift <- function(x, ...) {
  as.ts(x$data)
}

#' @rdname meanshift-generics
#' @inheritParams stats::logLik
#' @export
logLik.meanshift <- function(object, ...) {
  m <- length(object$tau)
  N <- nobs(object)
  ll <- -(N * log(object$sigma_hatsq) + N + N * log(2 * pi)) / 2
  params_estimated <- (m + 1) * (object$trends + 1)
  attr(ll, "real_params_estimated") <- params_estimated
  attr(ll, "df") <- m + params_estimated + 1 + (object$ar1)
  attr(ll, "nobs") <- N
  attr(ll, "tau") <- object$tau
  attr(ll, "ar1") <- object$ar1
  class(ll) <- "logLik"
  return(ll)
}

#' @rdname meanshift-generics
#' @export
nobs.meanshift <- function(object, ...) {
  object$nobs
}

#' @rdname meanshift-generics
#' @export
glance.meanshift <- function(x, ...) {
  tibble::tibble(
    pkg = "tidychangepoint",
    version = package_version(utils::packageVersion("tidychangepoint")),
    algorithm = x$model_name,
    params = list(ar1 = x$ar1),
    num_cpts = length(changepoints(x)),
    rmse = sqrt(mean(x$residuals^2)),
    logLik = as.double(logLik(x)),
    AIC = AIC(x),
    BIC = BIC(x),
    MBIC = MBIC(x),
    MDL = MDL(x)
  )
}

#' @rdname changepoints
#' @export
changepoints.meanshift <- function(x, ...) {
  x$tau |>
    as.integer()
}
