#' Generic methods for classes from [stats]
#' 
#' @inheritParams stats::logLik
#' @details
#'  loc.ind is the binary input of length N
#' Note `X[1]` cannot be a changepoint,
#' so `loc.ind[1]` is set to be zero
#' loc.ind = 0/1. 1 corresponds a CPT location, 0 is not.
#' @export
#' @author Xueheng Shi
#' @examples
#' logLik(as.ts(CET), loc.ind = round(runif(length(CET))))
#' logLik(as.ts(CET), loc.ind = tau2binary(c(42, 81, 330), n = length(CET)))
#' logLik(fit_meanshift(CET, tau = c(42, 81, 330)))
logLik.ts <- function(object, ...) {
  args <- list(...)
  if (is.null(args[["loc.ind"]])) {
    stop("ts method requires a loc.ind argument")
  } else {
    loc.ind <- args[["loc.ind"]]
  }
  Xt <- object
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
  
  ll <- -(N * log(sigma.hatsq) + N + N * log(2 * pi)) / 2
  attr(ll, "df") <- 2 * m + 3
  attr(ll, "nobs") <- N
  attr(ll, "tau") <- tau
  class(ll) <- "logLik"
  return(ll)
}




#' @rdname logLik.ts
#' @export
exceedances.ts <- function(x, ...) {
  exceedances(as.double(x))
}

#' @rdname logLik.ts
#' @param x A numeric vector
#' @export
exceedances.double <- function(x, ...) {
  which(x > mean(x, na.rm = TRUE, ...))
}

#' @rdname logLik.ts
#' @export
#' @examples
#' x <- HadCET$annual_mean_temp
#' MDL(logLik(as.ts(x), loc.ind = tau2binary(123, n = length(x))))
MDL.logLik <- function(object, ...) {
  tau <- attr(object, "tau")
  n <- nobs(object)
  penalty_mdl(pad_tau(tau, n)) - 2 * object |>
    as.double()
}

#' @rdname logLik.ts
#' @references Zhang and Seigmmund (2007) for MBIC: \doi{10.1111/j.1541-0420.2006.00662.x}
#' @export
MBIC.logLik <- function(object, ...) {
  tau <- attr(object, "tau")
  m <- length(tau)
  if (m == 0) {
    penalty <- 0
  } else {
    n <- nobs(x)
    padded_tau <- pad_tau(tau, n)
    penalty <- 3 * m * log(n) + sum(log(diff(padded_tau) / n)) 
  }
  penalty - 2 * object |>
    as.double()
}