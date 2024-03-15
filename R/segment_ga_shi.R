#' Segment a time series using a genetic algorithm
#' @param x A time series
#' @param ... arguments passed to [GA::ga()]
#' @export
#' @author Xueheng Shi
#' @examples
#' \dontrun{
#' res <- segment_ga_shi(CET, maxiter = 50)
#' summary(res)
#' str(res)
#' plot(res)
#' }

segment_ga_shi <- function(x, ...) {
  n <- length(x)
  
  obj_fun <- function(tau_vec_string) {
    ll <- logLik(as.ts(x), loc.ind = tau_vec_string)
    -BIC(ll)
  }
  
  GA::ga(
    type = "binary", 
    fitness = obj_fun,
    nBits = n,
    popSize = 200,
    ...
  )
}

