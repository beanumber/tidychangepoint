#' Segment a time series using a variety of algorithms
#' 
#' @description
#' A wrapper function that encapsulates various algorithms for detecting changepoint
#' sets in univariate time series. 
#' 
#' @param x a [tsibble::tsibble] or numeric vector coercible into a [stats::ts] object
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
#' @return an object of class `tidycpt`. Every `tidycpt` object contains:
#' - `segmenter`: The object returned by the underlying function. 
#' - `nhpp`: A `tbl_df` showing the best fit parameters for each region, as 
#'   defined by the chnagepoint set returned by `changepoints()`. These parameters
#'   are fit using the non-homogeneous Poisson process model in [fit_nhpp()].
#'   This object also has class `nhpp`
#' @details Currently, [segment()] can use the following algorithms, depending
#' on the value of the `method` argument:
#' - `cpt-pelt`: Uses the PELT algorithm as implemented in 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `cpt-binseg`: Uses the Binary Segmentation algorithm as implemented by 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `cpt-segneigh`: Uses the Segmented Neighborhood algorithm as implemented by 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `single-best`: Uses the AMOC criteria as implemented by 
#'   [changepoint::cpt.meanvar()]. The `segmenter` is of class `cpt`.
#' - `cpt-gbmdl`: Uses the Genetic BMDL heuristic as implemented by 
#'   [segment_gbmdl()]. The `segmenter` is of class [new_cpt_gbmdl].
#' - `manual`: Uses the vector of changepoints in the `cpts` arugment and
#'   [stats::lm()]. The `segmenter` is of class `seg_default`.
#' - `null`: The default. Uses [stats::lm()] with no changepoints. 
#'   The `segmenter` is of class `seg_default`.
#' [stats::lm], or [new_cpt_gbmdl]
#' @seealso [changepoint::cpt.meanvar()]
#' @examples
#' segment(DataCPSim, method = "cpt-pelt")
#' segment(DataCPSim, method = "cpt-pelt", penalty = "AIC")
#' segment(DataCPSim, method = "cpt-binseg", penalty = "AIC")
#' segment(DataCPSim, method = "cpt-segneigh", penalty = "BIC")
#' segment(DataCPSim, method = "random", num_generations = 10)
#' segment(DataCPSim, method = "manual", cpts = c(826))
#' two_cpts <- segment(DataCPSim, method = "manual", cpts = c(365, 826))
#' plot(two_cpts)
#' diagnose(two_cpts)
#' \dontrun{
#' x <- segment(DataCPSim, method = "cpt-gbmdl")
#' }
#' 

segment.ts <- function(x, method = "null", ...) {
  args <- list(...)
  message(paste("method:", method))
  n <- length(x)
  ds <- data.frame(y = x, t = 1:n)
  if (method == "cpt-pelt") {
    mod <- changepoint::cpt.meanvar(data = x, method = "PELT", ...)
  }
  if (method == "cpt-binseg") {
    mod <- changepoint::cpt.meanvar(data = x, method = "BinSeg", ...)
  }
  if (method == "cpt-segneigh") {
    mod <- changepoint::cpt.meanvar(data = x, method = "SegNeigh", ...)
  }
  if (method == "single-best") {
    mod <- changepoint::cpt.meanvar(data = x, method = "AMOC", ...)
  }
  if (method == "cpt-gbmdl") {
    mod <- segment_gbmdl(x, ...)
  }
  if (method == "random") {
    mod <- segment_random(x, ...)
  }
  if (method == "manual") {
    if(!"cpts" %in% names(args)) {
      stop("Please supply the cpts argument to use the manual algorithm.")
    }
    cpts <- args[["cpts"]]
    if (!is.list(cpts)) {
      cpts <- list(cpts)
    }
    mod <- new_seg_default(x, cpt_list = cpts)
  }
  if (method == "null") {
    mod <- new_seg_default(x)
  }
  # build the tidycpt object
  obj <- list(
    segmenter = mod,
    nhpp = fit_nhpp(as.ts(x), changepoints(mod))
  )
  class(obj) <- c("tidycpt")
  return(obj)
}
