#' Segment a time series using a genetic algorithm
#' @export
#' @examples
#' \dontrun{
#' res <- segment_ga(DataCPSim, initial_prob = 0.01, maxiter = 50)
#' summary(res)
#' str(res)
#' plot(res)
#' }

segment_ga <- function(x, initial_prob = 0.01, ...) {
  n <- length(x)
  
  obj_fun <- function(tau_binary_vec) {
    tau <- which(tau_binary_vec == 1)
    -BMDL(fit_nhpp(x, tau))
  }
  memoise::memoise(obj_fun)
  
  init_pop <- function(obj, p = initial_prob) {
    rbinom(obj@nBits * obj@popSize, size = 1, prob = p) |>
      matrix(ncol = obj@nBits)
  }
  
  GA::ga(
    type = "binary", 
    fitness = obj_fun,
    nBits = n,
    population = init_pop,
    ...
  )
}




#' @importClassesFrom GA ga
methods::setClass("tidyga", contains = "ga", slots = c(data = "ts"))

