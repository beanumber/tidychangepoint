#' Segment a time series using a genetic algorithm
#' 
#' @description
#' Segmenting functions for various genetic algorithms
#' 
#' @param x A time series
#' @param model_fn A `character` or `name` coercible into a [fun_cpt()] function. 
#' See, for example, [fit_meanshift_norm()].
#' @param penalty_fn A function that evaluates the changepoint set returned by
#' `model_fn`. We provide [AIC()], [BIC()], [MBIC()], [MDL()], and [BMDL()].
#' @param model_fn_args A [list()] of parameters passed to `model_fn`
#' @param ... arguments passed to [GA::ga()]
#' @export
#' @examples
#' \dontrun{
#' res <- segment_ga(DataCPSim, maxiter = 10)
#' summary(res)
#' str(res)
#' plot(res)
#' # Shi's algorithm
#' x <- segment(CET, method = "ga-shi", maxiter = 50)
#' str(x)
#' # GeneticBMDL
#' y <- segment(CET, method = "ga-coen", maxiter = 20)
#' changepoints(y)
#' 
#' z <- segment(CET, method = "ga-coen", maxiter = 10, model_fn_args = list(threshold = 2))
#' changepoints(z)
#' }

segment_ga <- function(x, 
                       model_fn = fit_meanshift_norm, 
                       penalty_fn = BIC, 
                       model_fn_args = list(), ...) {
  n <- length(as.ts(x))
  if (!inherits(model_fn, "fun_cpt")) {
    model_fn <- fun_cpt(model_fn)
  }
  if (!inherits(model_fn, "fun_cpt")) {
    stop("model_fn must be coercible into a fun_cpt")
  }
  
  obj_fun <- function(tau_binary_vec) {
    tau <- binary2tau(tau_binary_vec)
    
    tryCatch(
      -penalty_fn(model_fn(as.ts(x), tau, threshold = model_fn_args[["threshold"]])),
      error = function(cnd) -Inf
    )
  }
  memoise::memoise(obj_fun)
  
  mod_ga <- GA::ga(
    type = "binary", 
    fitness = obj_fun,
    nBits = n,
    ...
  )
  
  out <- methods::as(mod_ga, "tidyga")
  out@data <- as.ts(x)
  model_fn_args$model_fn <- model_name(model_fn)
  model_fn_args$penalty_fn <- penalty_fn |>
    body() |>
    as.character() |>
    purrr::pluck(2)
  out@model_fn_args <- model_fn_args
  return(out)
}

#' @rdname segment_ga
#' @export
#' @details
#'   - [segment_ga_shi()]: Shi's algorithm is the algorithm used in 
#'   \doi{10.1175/JCLI-D-21-0489.1}. 
#' Note that in order to achieve the reported results you have to run the algorithm
#' for a really long time. Pass the values `maxiter` = 50000 and `run` = 10000
#' to [GA::ga()] using the dots. 
#' @references Shi, et al. (2022, \doi{10.1175/JCLI-D-21-0489.1})
#' @examples
#' \dontrun{
#' # This will take a really long time!
#' x <- segment(CET, method = "ga-shi", maxiter = 500, run = 100)
#' changepoints(x)
#' 
#' y <- segment(CET, method = "ga", model_fn = fit_lmshift, penalty_fn = BIC, 
#'   popSize = 200, maxiter = 5000, run = 1000, model_fn_args = list(trends = TRUE), 
#'   population = build_gabin_population(CET))
#' 
#' }
#' 
segment_ga_shi <- function(x, ...) {
  segment_ga(
    x, model_fn = fit_meanshift_norm_ar1, penalty_fn = BIC, popSize = 200, ...
  )
}

#' @rdname segment_ga
#' @details
#'   - [segment_ga_coen()]: Coen's algorithm is the one used in 
#'   \doi{10.1007/978-3-031-47372-2_20}.
#' Note that the speed of the algorithm is highly sensitive to the size of the 
#' changepoint sets under consideration, with large changepoint sets being slow. 
#' Consider setting the `population` argument to [GA::ga()] to improve 
#' performance. Coen's algorithm uses the [build_gabin_population()] function
#' for this purpose by default. 
#' @seealso [build_gabin_population()]
#' @references Taimal, et al. (2023, \doi{10.1007/978-3-031-47372-2_20})
#' @export
#' @examples
#' \dontrun{
#' x <- segment(method = "ga-coen", maxiter = 50)
#' }
#' 
segment_ga_coen <- function(x, ...) {
  segment_ga(
    x, model_fn = fit_nhpp, penalty_fn = BMDL, 
    population = build_gabin_population(x), popSize = 50, ...
  )
}

#' @rdname segment_ga
#' @details
#'   - [segment_ga_random()]: Randomly select candidate changepoint sets. This 
#'   is implemented as a genetic algorithm with only one generation (i.e., 
#'   `maxiter = 1`. Note
#'   that this function uses [log_gabin_population()] by default. 
#' 
#' @export
#' @seealso [log_gabin_population()]
#' @examples
#' x <- segment(CET, method = "random")
segment_ga_random <- function(x, ...) {
  segment_ga(
    x, population = log_gabin_population(x), maxiter = 1, ...
  )
}


#' @importClassesFrom GA ga
methods::setClass(
  "tidyga", 
  contains = "ga", 
  slots = c(data = "ts", model_fn_args = "list")
)

