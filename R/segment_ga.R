#' Segment a time series using a genetic algorithm
#' @export
#' @examples
#' res <- segment_ga(DataCPSim, maxiter = 20)
#' summary(res)
#' str(res)
#' plot(res)

segment_ga <- function(x, ...) {
  fun <- function(tau_binary_vec) {
    tau <- which(tau_binary_vec == 1)
    -BMDL(fit_nhpp(x, tau))
  }
  GA::ga(
    type = "binary", 
    fitness = fun,
    nBits = length(x),
    ...
  )
}


#' @importClassesFrom GA ga
methods::setClass("tidyga", contains = "ga", slots = c(data = "ts"))

