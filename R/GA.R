#' Compatibility layer for GA
#' @param x A `GA` object returned by [GA::ga()]
#' @param ... arguments passed to methods
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 10)
#' class(cpts)
#' y <- augment(cpts)
#' class(y)
#' y
#' tidy(cpts)
#' glance(cpts)

glance.ga <- function(x, ...) {
  tibble::tibble(
    pkg = "ga",
    version = package_version(utils::packageVersion("GA")),
    algorithm = "Genetic",
    params = list(params(x)),
    num_cpts = length(changepoints(x))
  )
}

#' @rdname glance.ga
#' @export
params.ga <- function(x, ...) {
  list(
    popSize = x@popSize,
    iter = x@iter,
    elitism = x@elitism,
    pcrossover = x@pcrossover,
    pmutation = x@pmutation
  )
}

#' @rdname glance.ga
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 5)
#' as.ts(cpts)
#' 
as.ts.ga <- function(x, ...) {
  x@data
}

#' @rdname glance.ga
#' @param object A `ga` object.
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 5)
#' nobs(cpts)
#' 
nobs.ga <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname glance.ga
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 5)
#' logLik(cpts)
#' 
logLik.ga <- function(object, ...) {
  logLik(as.ts(object), loc.ind = tau2binary(changepoints(object)))
}

#' @rdname glance.ga
#' @details
#'  loc.ind is the binary input of length N
#' Note `X[1]` cannot be a changepoint,
#' so `loc.ind[1]` is set to be zero
#' loc.ind = 0/1. 1 corresponds a CPT location, 0 is not.
#' @export
#' @author Xueheng Shi
#' @examples
#' x <- england_temperature$annual_mean_temp
#' logLik(as.ts(x), loc.ind = round(runif(length(x))))
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
  class(ll) <- "logLik"
  return(ll)
}

#' @rdname glance.ga
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "ga", maxiter = 5)
#' changepoints(cpts$segmenter)
#' 
changepoints.ga <- function(x, ...) {
  which(x@solution == 1)
}
