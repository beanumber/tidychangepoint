#' Segment a time series using a genetic algorithm
#' @param x A time series
#' @param model_fn A function that takes two arguments: `x` (a time series) and 
#' `tau` (a set of changepoint indices). See [fit_meanshift_ar1()], 
#' [fit_lmshift()], and [fit_nhpp()]
#' @param penalty_fn A function that evaluates the changepoint set returned by
#' `model_fn`. We provide [AIC()], [BIC()], [MBIC()], [MDL()], and [BMDL()].
#' @param initial_prob Initial probability of being selected
#' @param ... arguments passed to [GA::ga()]
#' @export
#' @examples
#' \dontrun{
#' res <- segment_ga(DataCPSim, initial_prob = 0.01, maxiter = 50)
#' summary(res)
#' str(res)
#' plot(res)
#' # Shi's algorithm
#' x <- segment(CET, method = "ga", penalty_fn = BIC, initial_prob = 0.06, maxiter = 50, popSize = 200)
#' str(x)
#' }

segment_ga <- function(x, model_fn = fit_meanshift_ar1, penalty_fn = BIC, initial_prob = 0.01, ...) {
  n <- length(x)
  
  obj_fun <- function(tau_binary_vec) {
    tau <- binary2tau(tau_binary_vec)
    -penalty_fn(model_fn(x, tau))
  }
  memoise::memoise(obj_fun)
  
  init_pop <- function(obj, p = initial_prob) {
    stats::rbinom(obj@nBits * obj@popSize, size = 1, prob = p) |>
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

