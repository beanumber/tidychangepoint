#' Segment a time series using a genetic algorithm
#' @export
#' @examples
#' \dontrun{
#' res <- segment_ga(DataCPSim, maxiter = 10)
#' summary(res)
#' str(res)
#' plot(res)
#' }

segment_ga <- function(x, ...) {
  
  obj_fun <- function(tau_binary_vec) {
    tau <- which(tau_binary_vec == 1)
    -BMDL(fit_nhpp(x, tau))
  }
  
  init_pop <- function(obj, p = 0.06) {
    rbinom(length(x) * 50, size = 1, prob = p) |>
      matrix(ncol = length(x))
  }
  
  GA::ga(
    type = "binary", 
    fitness = obj_fun,
    nBits = length(x),
    population = init_pop,
    ...
  )
}




#' @importClassesFrom GA ga
methods::setClass("tidyga", contains = "ga", slots = c(data = "ts"))

