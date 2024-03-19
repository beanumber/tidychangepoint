#' Fast implementation of meanshift model
#' @inheritParams stats::logLik
#' @details
#'  loc.ind is the binary input of length N
#' Note `X[1]` cannot be a changepoint,
#' so `loc.ind[1]` is set to be zero
#' loc.ind = 0/1. 1 corresponds a CPT location, 0 is not.
#' @export
#' @author Xueheng Shi
#' @examples
#' tau <- c(365, 826)
#' mod <- fit_meanshift_ar1(DataCPSim, loc.ind = tau2binary(tau, n = length(DataCPSim)))
#' logLik(mod)
#' deg_free(mod)
#' 
#' cpts <- c(1700, 1739, 1988)
#' ids <- time2tau(cpts, lubridate::year(time(CET)))
#' mod <- fit_meanshift_ar1(CET, loc.ind = tau2binary(ids, n = length(CET)))
#' glance(mod)

fit_meanshift_ar1 <- function(object, ...) {
  args <- list(...)
  if (is.null(args[["loc.ind"]])) {
    stop("ts method requires a loc.ind argument")
  } else {
    loc.ind <- args[["loc.ind"]]
  }
  Xt <- as.ts(object)
  loc.ind[1] <- 0
  N <- length(Xt) # length of the series
  m <- sum(loc.ind) # Number of CPTs
  
  if (m == 0) {
    ## Case 1, Zero Changepoint
    tau <- NA
    mu.hat <- mean(Xt)
    phi.hat <- sum((Xt - mu.hat)[-N] * (Xt - mu.hat)[-1]) / sum((Xt - mu.hat)[-1]^2)
    Xt.hat <- c(mu.hat, mu.hat + phi.hat * (Xt[-N] - mu.hat))
  } else {
    tau.vec <- loc.ind * (1:N) # convert binary to CPT location
    tau <- tau.vec[tau.vec > 0] # keep CPT locations only
    tau.ext <- c(1, tau, (N + 1)) # include CPT boundary 1 and N+1
    
    ## Split Xt to regimes/segments to
    ## compute phi.hat and sigma.hat.sq
    seg.len <- diff(tau.ext) # length of each segments
    ff <- rep(0:m, times = seg.len) ## create factors for segmentation
    Xseg <- split(Xt, ff) ## Segmentation list
    mu.seg <- unlist(lapply(Xseg, mean), use.names = F)
    mu.hat <- rep(mu.seg, seg.len)
    phi.hat <- sum((Xt - mu.hat)[-N] * (Xt - mu.hat)[-1]) / sum((Xt - mu.hat)[-1]^2)
    Xt.hat <- c(mu.hat[1], mu.hat[-1] + phi.hat * (Xt[-N] - mu.hat[-N]))
  }
  sigma.hatsq <- sum((Xt - Xt.hat)^2) / N
  out <- list(
    means = mu.seg,
    fitted.values = Xt.hat,
    residuals = Xt - Xt.hat,
    sigma_hatsq = sigma.hatsq,
    tau = tau,
    model_name <- "meanshift_ar1",
    ar1 = TRUE,
    trends = FALSE
  )
  class(out) <- c("meanshift", class(out))
  return(out)
}

#' @rdname fit_meanshift_ar1
#' @inheritParams stats::logLik
#' @export
logLik.meanshift <- function(object, ...) {
  m <- length(object$tau)
  N <- length(object$fitted.values)
  ll <- -(N * log(object$sigma_hatsq) + N + N * log(2 * pi)) / 2
  attr(ll, "df") <- 2 * m + 3
  attr(ll, "nobs") <- N
  attr(ll, "tau") <- tau
  class(ll) <- "logLik"
  return(ll)
}

#' @rdname fit_meanshift_ar1
#' @export
MBIC.meanshift <- function(object, ...) {
  MBIC(logLik(object))
}

#' @rdname fit_meanshift_ar1
#' @export
MDL.meanshift <- function(object, ...) {
  MDL(logLik(object))
}

#' @rdname fit_meanshift_ar1
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

#' @rdname fit_meanshift_ar1
#' @export
changepoints.meanshift <- function(x, ...) {
  x$tau |>
    as.integer()
}
