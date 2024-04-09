#' Segment a time series using a variety of algorithms
#' 
#' @description
#' A wrapper function that encapsulates various algorithms for detecting changepoint
#' sets in univariate time series. 
#' 
#' @param x a numeric vector coercible into a [stats::ts] object
#' @param method a character string indicating the algorithm to use. See Details.
#' @param ... arguments passed to methods
#' @export
#' @examples
#' mod_null <- segment(DataCPSim)
#' augment(mod_null)
#' tidy(mod_null)
#' glance(mod_null)

segment <- function(x, method = "null", ...) UseMethod("segment")

#' @rdname segment
#' @export
segment.tbl_ts <- function(x, method = "null", ...) {
  if (!stats::is.ts(stats::as.ts(x))) {
    stop("x is not coercible into a ts object.")
  }
  segment(as.ts(x), method = method, ... = ...)
  out$time_index <- time(as.ts(x))
  return(out)
}

#' @rdname segment
#' @export
segment.xts <- function(x, method = "null", ...) {
  if (!stats::is.ts(stats::as.ts(x))) {
    stop("x is not coercible into a ts object.")
  }
  out <- segment(as.ts(x), method = method, ... = ...)
  out$time_index <- time(x)
  return(out)
}

#' @rdname segment
#' @export
segment.numeric <- function(x, method = "null", ...) {
  if (!stats::is.ts(stats::as.ts(x))) {
    stop("x is not coercible into a ts object.")
  }
  segment(as.ts(x), method = method, ... = ...)
}

#' @rdname segment
#' @export
#' @return An object of class `tidycpt`. Every `tidycpt` object contains:
#' - `segmenter`: The object returned by the underlying function. 
#' - `nhpp`: A `tbl_df` showing the best fit parameters for each region, as 
#'   defined by the changepoint set returned by [changepoints()] when called 
#'   on the `segmenter`. These parameters
#'   are fit using the non-homogeneous Poisson process model in [fit_nhpp()].
#'   This object also has class `nhpp`.
#' - `elapsed_time`: The clock time that passed while the algorithm was running.
#' @details Currently, [segment()] can use the following algorithms, depending
#' on the value of the `method` argument:
#' - `pelt`: Uses the PELT algorithm as implemented in 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `binseg`: Uses the Binary Segmentation algorithm as implemented by 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `segneigh`: Uses the Segmented Neighborhood algorithm as implemented by 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `single-best`: Uses the AMOC criteria as implemented by 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `wbs`: Uses the Wild Binary Segmentation algorithm as implemented by 
#'   [wbs::wbs()]. The `segmenter` is of class `wbs`.
#' - `ga`: Uses the Genetic algorithm implemented by 
#'   [GA::ga()]. The `segmenter` is of class `tidyga`.
#' - `taimal`: Uses the Genetic BMDL heuristic as implemented by 
#'   [segment_taimal()]. The `segmenter` is of class [seg_default()].
#' - `manual`: Uses the vector of changepoints in the `tau` argument. 
#'   The `segmenter` is of class [seg_default()]`.
#' - `null`: The default. Uses no changepoints. 
#'   The `segmenter` is of class [seg_default()]`.
#' @seealso [changepoint::cpt.meanvar()], [wbs::wbs()], [GA::ga()]
#' @examples
#' segment(DataCPSim, method = "pelt")
#' segment(DataCPSim, method = "pelt", penalty = "AIC")
#' segment(DataCPSim, method = "binseg", penalty = "AIC")
#' segment(DataCPSim, method = "segneigh", penalty = "BIC")
#' segment(DataCPSim, method = "random", num_generations = 10)
#' segment(DataCPSim, method = "manual", tau = c(826))
#' two_cpts <- segment(DataCPSim, method = "manual", tau = c(365, 826))
#' plot(two_cpts)
#' diagnose(two_cpts)
#' segment(bogota_pm, method = "pelt")
#' \dontrun{
#' x <- segment(DataCPSim, method = "gbmdl", num_generations = 10)
#' }
#' 

segment.ts <- function(x, method = "null", ...) {
  args <- list(...)
  message(paste("method:", method))
  begin <- Sys.time()
  
  if (method == "pelt") {
    seg <- changepoint::cpt.meanvar(data = x, method = "PELT", ...)
  }
  if (method == "binseg") {
    seg <- changepoint::cpt.meanvar(data = x, method = "BinSeg", ...)
  }
  if (method == "segneigh") {
    seg <- changepoint::cpt.meanvar(data = x, method = "SegNeigh", ...)
  }
  if (method == "single-best") {
    seg <- changepoint::cpt.meanvar(data = x, method = "AMOC", ...)
  }
  if (method == "wbs") {
    seg <- wbs::wbs(x, ...)
  }
  if (method == "ga") {
    seg <- segment_ga(x, ...)
  }
  if (method == "ga-shi") {
    seg <- segment_ga_shi(x, ...)
  }
  if (method == "ga-taimal") {
    seg <- segment_ga_taimal(x, ...)
  }
  if (method == "taimal") {
    seg <- segment_taimal(x, ...)
  }
  if (method == "random") {
    seg <- segment_random(x, ...)
  }
  if (method == "manual") {
    if(!"tau" %in% names(args)) {
      stop("Please supply the tau argument to use the manual algorithm.")
    }
    tau <- args[["tau"]]
    if (!is.list(tau)) {
      tau <- list(tau)
    }
    seg <- new_seg_default(x, cpt_list = tau)
  }
  if (method == "null") {
    seg <- new_seg_default(x)
  }
  # build the tidycpt object
  m <- model_fit(seg)
  obj <- list(
    segmenter = seg,
    model = m(as.ts(x), changepoints(seg), ...),
    elapsed_time = Sys.time() - begin
  )
  class(obj) <- c("tidycpt")
  return(obj)
}
