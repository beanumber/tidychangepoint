#' Segment a time series using a genetic algorithm
#' 
#' @description
#' Segmenting functions for various genetic algorithms
#' 
#' @param x A time series
#' @param ... arguments passed to [changepointGA::cptga()]
#' @details
#' [segment_cptga()] uses the genetic algorithm in [changepointGA::cptga()] to "evolve" a random
#' set of candidate changepoint sets, using the penalized objective function
#' specified by `penalty_fn`. 
#' By default, the normal `meanshift` model is fit (see [fit_meanshift_norm()])
#' and the [BIC] penalty is applied.  
#' 
#' @returns A `tidycptga` object. This is just a [changepointGA::cptga()] 
#' object with an additional
#' slot for `data` (the original time series).
#' @export
#' @examples
#' \donttest{
#' # Segment a time series using a genetic algorithm
#' res <- segment_cptga(CET)
#' summary(res)
#' 
#' # Segment a time series using changepointGA
#' x <- segment(CET, method = "cptga")
#' summary(x)
#' changepoints(x)
#' }

segment_cptga <- function(x, ...) {
  n <- length(as.ts(x))

  out <- changepointGA::cptga(
    ObjFunc = changepointGA::ARIMA.BIC, 
    N = n, 
    XMat = matrix(1, nrow = n, ncol = 1), 
    Xt = x,
#    ... = ...
  )
  
  out <- methods::as(out, "tidycptga")
  out@data <- as.ts(x)
  out@model_fn_args <- list()
  return(out)
}

#' @importClassesFrom changepointGA cptga
methods::setClass(
  "tidycptga", 
  contains = "cptga", 
  slots = c(data = "ts", model_fn_args = "list")
)
