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
#' res <- segment_ga(DataCPSim, model_params = list(initial_prob = 0.06), maxiter = 50)
#' summary(res)
#' str(res)
#' plot(res)
#' # Shi's algorithm
#' x <- segment(CET, method = "ga", penalty_fn = BIC, 
#' model_params = list(initial_prob = 0.06), maxiter = 50, popSize = 200)
#' str(x)
#' # GeneticBMDL
#' y <- segment(CET, method = "ga", model_fn = fit_nhpp, penalty_fn = BMDL, 
#'   model_params = list(initial_prob = 0.06), maxiter = 20, popSize = 50)
#' changepoints(y)
#' 
#' z <- segment(CET, method = "ga", model_fn = fit_nhpp, penalty_fn = BMDL, 
#'   maxiter = 10, popSize = 20,
#'   model_params = list(initial_prob = 0.06, threshold = 2))
#' changepoints(z)
#' }

segment_ga <- function(x, 
                       model_fn = fit_meanshift_ar1, 
                       penalty_fn = BIC, 
                       model_params = list(initial_prob = 0.01), ...) {
  n <- length(x)
  
  obj_fun <- function(tau_binary_vec) {
    tau <- binary2tau(tau_binary_vec)
    -penalty_fn(model_fn(x, tau, threshold = model_params[["threshold"]]))
  }
  memoise::memoise(obj_fun)
  
  init_pop <- function(obj, p = model_params[["initial_prob"]]) {
#    message("p:", p)
    stats::rbinom(obj@nBits * obj@popSize, size = 1, prob = p) |>
      matrix(ncol = obj@nBits)
  }
  
  mod_ga <- GA::ga(
    type = "binary", 
    fitness = obj_fun,
    nBits = n,
    population = init_pop,
    ...
  )
  
  out <- methods::as(mod_ga, "tidyga")
  out@data <- x
  out@model_params <- model_params
  return(out)
}




#' @importClassesFrom GA ga
methods::setClass(
  "tidyga", 
  contains = "ga", 
  slots = c(data = "ts", model_params = "list")
)

