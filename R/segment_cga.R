#' Segment a time series using a genetic algorithm
#' 
#' @description
#' Segmenting functions for various genetic algorithms
#' 
#' @param x A time series
#' @param ... arguments passed to [changepointGA::GA()]
#' @details
#' [segment_cga()] uses the genetic algorithm in [GA::ga()] to "evolve" a random
#' set of candidate changepoint sets, using the penalized objective function
#' specified by `penalty_fn`. 
#' By default, the normal `meanshift` model is fit (see [fit_meanshift_norm()])
#' and the [BIC] penalty is applied.  
#' 
#' @returns A `cga` object. This is just a [changepointGA::GA()] 
#' object with an additional
#' slot for `data` (the original time series).
#' @export
#' @examples
#' \donttest{
#' # Segment a time series using a genetic algorithm
#' res <- segment_cga(CET)
#' summary(res)
#' 
#' # Segment a time series using changepointGA
#' x <- segment(CET, method = "cga")
#' summary(x)
#' changepoints(x)
#' }

segment_cga <- function(x, ...) {
  n <- length(as.ts(x))

  out <- changepointGA::GA(
    ObjFunc = changepointGA::ARIMA.BIC, 
    N = n, 
    XMat = matrix(1, nrow = n, ncol = 1), 
    Xt = x,
    ... = ...
  )
  
  out$data <- as.ts(x)
  class(out) <- "cga"
  return(out)
}

