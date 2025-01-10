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
#' 
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
#' @return An object of class [tidycpt-class]. 
#' @details Currently, [segment()] can use the following algorithms, depending
#' on the value of the `method` argument:
#' - `pelt`: Uses the PELT algorithm as implemented in 
#'   [segment_pelt()], which wraps either [changepoint::cpt.mean()] or 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `binseg`: Uses the Binary Segmentation algorithm as implemented by 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `segneigh`: Uses the Segmented Neighborhood algorithm as implemented by 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `single-best`: Uses the AMOC criteria as implemented by 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `wbs`: Uses the Wild Binary Segmentation algorithm as implemented by 
#'   [wbs::wbs()]. The `segmenter` is of class `wbs`.
#' - `ga`: Uses the Genetic algorithm implemented by [segment_ga()], which wraps
#'   [GA::ga()]. The `segmenter` is of class `tidyga`.
#' - `ga-shi`: Uses the genetic algorithm implemented by [segment_ga_shi()], 
#'   which wraps
#'   [segment_ga()]. The `segmenter` is of class `tidyga`.
#' - `ga-coen`: Uses Coen's heuristic as implemented by [segment_ga_coen()]. 
#'   The `segmenter` is of class `tidyga`. This implementation supersedes the
#'   following one.
#' - `coen`: Uses Coen's heuristic as implemented by 
#'   [segment_coen()]. The `segmenter` is of class [seg_basket()]. Note that 
#'   this function is deprecated. 
#' - `random`: Uses a random basket of changepoints as implemented by 
#'   [segment_ga_random()]. 
#'   The `segmenter` is of class `tidyga`. 
#' - `manual`: Uses the vector of changepoints in the `tau` argument. 
#'   The `segmenter` is of class [seg_cpt]`.
#' - `null`: The default. Uses no changepoints. 
#'   The `segmenter` is of class [seg_cpt].
#' @seealso [changepoint::cpt.meanvar()], [wbs::wbs()], [GA::ga()], 
#' [segment_ga()]
#' @examples
#' # Segment a time series using PELT
#' segment(DataCPSim, method = "pelt")
#' 
#' # Segment a time series using PELT and the BIC penalty
#' segment(DataCPSim, method = "pelt", penalty = "BIC")
#' 
#' # Segment a time series using Binary Segmentation
#' segment(DataCPSim, method = "binseg", penalty = "BIC")
#' 
#' # Segment a time series using a random changepoint set
#' segment(DataCPSim, method = "random")
#' 
#' # Segment a time series using a manually-specified changepoint set
#' segment(DataCPSim, method = "manual", tau = c(826))
#' 
#' # Segment a time series using a null changepoint set
#' segment(DataCPSim)
#' 
segment.ts <- function(x, method = "null", ...) {
  args <- list(...)
#  message(paste("method:", method))
  begin <- Sys.time()
  
  if (method == "pelt") {
    seg <- segment_pelt(x, ...)
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
  if (method == "ga-coen") {
    seg <- segment_ga_coen(x, ...)
  }
  if (method == "coen") {
    seg <- segment_coen(x, ...)
  }
  if (method == "random") {
    seg <- segment_ga_random(x, ...)
  }
  if (method == "manual") {
    if(!"tau" %in% names(args)) {
      stop("Please supply the tau argument to use the manual algorithm.")
    }
    tau <- args[["tau"]]
    if (!is.list(tau)) {
      tau <- list(tau)
    }
    seg <- segment_manual(x, ...)
  }
  if (method == "null") {    
    seg <- segment_manual(x, tau = NULL)
  }
  # build the tidycpt object
  obj <- list(
    segmenter = seg,
    model = as.model(seg),
    elapsed_time = Sys.time() - begin
  )
  class(obj) <- c("tidycpt")
  return(obj)
}
