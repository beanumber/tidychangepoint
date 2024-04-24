#' Extract changepoints
#' 
#' @param x A `tidycpt` object
#' @param ... arguments passed to methods
#' @description
#' Not to be confused with [wbs::changepoints()], which returns different
#' information. 
#' 
#' @seealso [wbs::changepoints()]
#' @family tidychangepoint-generics
#' @export
changepoints <- function(x, ...) UseMethod("changepoints")

#' @rdname changepoints
#' @export
changepoints.default <- function(x, ...) {
  attr(x, "cpt_true")
}

#' Compute exceedances of a threshold for a time series
#' 
#' @inheritParams segment
#' @family tidychangepoint-generics
#' @export
exceedances <- function(x, ...) UseMethod("exceedances")

#' @rdname exceedances
#' @export
exceedances.default <- function(x, ...) {
  exceedances(as.ts(x), ...)
}

#' Retrieve the optimal fitness (or objective function) value used by an algorithm
#' @inheritParams as.model
#' @family tidychangepoint-generics
#' @export
fitness <- function(object, ...) UseMethod("fitness")

#' @rdname changepoints
#' @family tidychangepoint-generics
#' @export
params <- function(x, ...) UseMethod("params")

#' Retrieve the name of the model that a segmenter or model used
#' 
#' @details
#' Every segmenter works by fitting a model to the data. [model_name()] returns
#' the name of a model that can be passed to [model_fit()] to retrieve the 
#' model fitting function. These functions must begin with the prefix `fit_`. 
#' Note that the model fitting functions exist in `tidychangepoint` are are
#' not necessarily the actual functions used by the segmenter. 
#' 
#' Models also implement `model_name()`. 
#' 
#' @return - [model_name()]: A `character` vector of length 1.
#' @inheritParams fitness
#' @export
#' @family modeling
#' @examples
#' x <- segment(CET, method = "pelt")
#' model_name(x$segmenter)
#' model_fit(model_name(x))
#' model_name(x$segmenter)
#' model_name(x$model)
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

#' Retrieve the arguments that a model-fitting function used
#' 
#' @details
#' Every segmenter uses a model-fitting function, and these functions sometimes
#' take arguments. [model_args()] recovers the arguments that were passed to 
#' the model fitting function when it was called. These are especially 
#' important when using a genetic algorithm. 
#' 
#' @inheritParams fitness
#' @export
#' @return - [model_args()]: A named `list` of arguments, or `NULL`
#' @family modeling
#' @examples
#' x <- segment(CET, method = "ga-taimal", maxiter = 3)
#' model_args(x$segmenter)
model_args <- function(object, ...) UseMethod("model_args")

#' @rdname model_args
#' @export
model_args.default <- function(object, ...) {
  object$model_fn_args
}

#' Convert a segmenter to a model object
#' @param object A segmenter object, typically returned by [segment()]
#' @param ... currently ignored
#' @export
as.model <- function(object, ...) UseMethod("as.model")

#' @rdname as.model
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
#' @family penalty-functions
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
#' @family penalty-functions
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
#' @family penalty-functions
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
#' @family tidychangepoint-generics
#' @export
diagnose <- function(x, ...) UseMethod("diagnose")