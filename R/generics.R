#' Extract changepoints
#' 
#' @param x A `tidycpt` object
#' @param ... arguments passed to methods
#' @description
#' Not to be confused with [wbs::changepoints()], which returns different
#' information. 
#' 
#' @seealso [wbs::changepoints()]
#' @export
changepoints <- function(x, ...) UseMethod("changepoints")

#' Compute exceedances of a threshold for a time series
#' 
#' @inheritParams segment
#' @export
exceedances <- function(x, ...) UseMethod("exceedances")

#' @rdname exceedances
#' @export
exceedances.default <- function(x, ...) {
  exceedances(as.ts(x), ...)
}

#' @rdname changepoints
#' @export
params <- function(x, ...) UseMethod("params")

#' Modified Bayesian Information Criterion
#' 
#' @inheritParams stats::logLik
#' @export
#' @seealso [stats::BIC()]
#' @references Zhang and Seigmmund (2007) for MBIC: \doi{10.1111/j.1541-0420.2006.00662.x}
MBIC <- function(object, ...) UseMethod("MBIC")

#' @rdname MBIC
#' @export
MBIC.default <- function(object, ...) {
  MBIC(logLik(object))
}

#' Maximum Descriptive Length
#' 
#' @export
MDL <- function(object, ...) UseMethod("MDL")

#' @rdname MDL
#' @export
MDL.default <- function(object, ...) {
  MDL(logLik(object))
}

#' Bayesian Maximum Descriptive Length
#' 
#' @inheritParams stats::logLik
#' @seealso [MDL()]
#' @export
BMDL <- function(object, ...) UseMethod("BMDL")

#' @rdname BMDL
#' @export
#' @examples
#' x <- segment(DataCPSim, method = "pelt")
#' BMDL(x$nhpp)
#' y <- segment(DataCPSim, method = "manual", cpts = 826)
#' BMDL(y$nhpp)
#' z <- segment(DataCPSim, method = "single-best")
#' BMDL(z$nhpp)
BMDL.default <- function(object, ...) {
  BMDL(logLik(object))
}

#' @rdname new_seg_default
#' @export
evaluate_cpts <- function(x, ...) UseMethod("evaluate_cpts")

#' Diagnose the fit of a segmented time series
#' @param x A `tidycpt` object
#' @param ... currently ignored
#' @export
diagnose <- function(x, ...) UseMethod("diagnose")