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

#' Retrieve the optimal fitness (or objective function) value used by an algorithm
#' @param object An object, typically returned by [segment()]
#' @param ... currently ignored
#' @export
fitness <- function(object, ...) UseMethod("fitness")

#' @rdname changepoints
#' @export
params <- function(x, ...) UseMethod("params")

#' Retrieve the model that a segmenter used
#' @inheritParams fitness
#' @export
model_name <- function(object, ...) UseMethod("model_name")

#' @rdname model_name
#' @export
model_name.default <- function(object, ...) {
  attr(object, "model_name")
}

#' @rdname model_name
#' @export
model_name.character <- function(object, ...) {
  object
}

#' Retrieve the model arguments that a segmenter used
#' @inheritParams fitness
#' @export
model_args <- function(object, ...) UseMethod("model_args")

#' @rdname model_name
#' @export
model_args.default <- function(object, ...) {
  object$model_fn_args
}

#' Convert a segmenter to a model object
#' @inheritParams fitness
#' @export
as.model <- function(object, ...) UseMethod("as.model")

#' @rdname ga-generics
#' @export
#' @examples
#' cpt <- segment(DataCPSim, method = "ga", maxiter = 5)
#' as.model(cpt$segmenter)
#' cpt <- segment(DataCPSim, method = "ga-taimal", model_fn_args = list(threshold = 80), maxiter = 5)
#' as.model(cpt$segmenter)$model_params
as.model.default <- function(object, ...) {
  f <- model_fit(object)
  args <- c(list(as.ts(object), tau = changepoints(object)), model_args(object), list(...))
  do.call(f, args)
}

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
#' BMDL(fit_nhpp(DataCPSim, tau = NULL))
#' BMDL(fit_nhpp(DataCPSim, tau = c(365, 830)))
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