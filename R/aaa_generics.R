#' Extract changepoints
#' @param x A [tidycpt-class], `segmenter`, or [mod_cpt] object
#' @param ... arguments passed to methods
#' @description
#' Retrieve the indices of the changepoints identified by an algorithm or model.
#' 
#' @details
#' [tidycpt-class] objects, as well as their `segmenter` and `model` components,
#' implement [changepoints()] methods. 
#' 
#' Note that this function is not to be confused with [wbs::changepoints()], 
#' which returns different information. 
#' @returns a numeric vector of changepoint indices, or, if `use_labels` is 
#' `TRUE`, a `character` of time labels. 
#' @seealso [wbs::changepoints()]
#' @family tidycpt-generics
#' @export
changepoints <- function(x, ...) UseMethod("changepoints")

#' @rdname changepoints
#' @details
#' For the `default` method, [changepoints()] will attempt to return the 
#' `cpt_true` attribute, which is set by [test_set()].
#' 
#' @export
changepoints.default <- function(x, ...) {
  attr(x, "cpt_true")
}

#' Convert, retrieve, or verify a segmenter object
#' @param object A [tidycpt-class] or `segmenter` object
#' @param ... Arguments passed to methods
#' @details
#' [tidycpt-class] objects have a `segmenter` component (that is typically
#' created by a class to [segment()]). 
#' The functions documented here are convenience utility functions
#' for working with the `segmenter` components. 
#' [as.segmenter()] is especially useful in pipelines to avoid having to use 
#' the `$` or `[` notation for subsetting.
#' 
#' [as.segmenter()] simply returns the segmenter of a `tidycpt` object.
#' @family tidycpt-generics
#' @returns
#'   - [as.segmenter()] returns the `segmenter` object of a `tidycpt` object. 
#'   Note that this could be of
#'   any class, depending on the class returned by the segmenting function.
#' @export
as.segmenter <- function(object, ...) UseMethod("as.segmenter")

#' @rdname as.segmenter
#' @details
#' [as.seg_cpt()] takes a wild-caught `segmenter` object of arbitrary class 
#' and converts it into a [seg_cpt] object. 
#' @family segmenter-functions
#' @return
#'   - [as.seg_cpt()] returns a [seg_cpt] object
#' @export
as.seg_cpt <- function(object, ...) UseMethod("as.seg_cpt")

#' Compute exceedances of a threshold for a time series
#' 
#' @inheritParams segment
#' @returns An ordered `integer` vector giving the indices of the values of `x`
#' that exceed the `threshold`.
#' @export
exceedances <- function(x, ...) UseMethod("exceedances")

#' @rdname exceedances
#' @export
exceedances.default <- function(x, ...) {
  exceedances(as.ts(x), ...)
}

#' Retrieve the optimal fitness (or objective function) value used by an 
#' algorithm
#' @param object A `segmenter` object.
#' @param ... currently ignored
#' @details
#' 
#' Segmenting algorithms use a **fitness** metric, typically through the use of
#' a penalized objective function, to determine which changepoint sets are more
#' or less optimal. 
#' This function returns the value of that metric for the changepoint set 
#' implied by the object provided. 
#' 
#' @family tidycpt-generics
#' @family segmenter-functions
#' @returns A named `double` vector with the fitness value.
#' @export
fitness <- function(object, ...) UseMethod("fitness")

#' Retrieve parameters from a segmenter
#' @inheritParams fitness
#' @details
#' Most segmenting algorithms have parameters. 
#' This function retrieves an informative set of those parameter values.
#' @returns A named `list` of parameters with their values. 
#' 
#' @family segmenter-functions
#' @export
seg_params <- function(object, ...) UseMethod("seg_params")

#' Retrieve the name of the model that a segmenter or model used
#' 
#' @details
#' Every segmenter works by fitting a model to the data. [model_name()] returns
#' the name of a model that can be passed to [whomademe()] to retrieve the 
#' model fitting function. These functions must begin with the prefix `fit_`. 
#' Note that the model fitting functions exist in `tidychangepoint` are are
#' not necessarily the actual functions used by the segmenter. 
#' 
#' Models also implement `model_name()`. 
#' 
#' @return A `character` vector of length 1.
#' @inheritParams fitness
#' @export
#' @family model-fitting
#' @family tidycpt-generics
#' @examples
#' # Segment a time series using PELT
#' x <- segment(CET, method = "pelt")
#' 
#' # Retrieve the name of the model from the segmenter
#' x |>
#'   as.segmenter() |>
#'   model_name()
#' 
#' # What function created the model? 
#' x |>
#'   model_name() |>
#'   whomademe()
#' model_name(x$segmenter)
#' 
#' # Retrieve the name of the model from the model
#' x |>
#'   as.model() |>
#'   model_name()
#'   
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
#' Every model is fit by a model-fitting function, and these functions sometimes
#' take arguments. 
#' [model_args()] recovers the arguments that were passed to 
#' the model fitting function when it was called. 
#' These are especially 
#' important when using a genetic algorithm. 
#' 
#' @inheritParams fitness
#' @export
#' @return A named `list` of arguments, or `NULL`
#' @family model-fitting
#' @family segmenter-functions
#' @examples
#' # Segment a time series using Coen's algorithm
#' x <- segment(CET, method = "ga-coen", maxiter = 3)
#' 
#' # Recover the arguments passed to the model-fitting function
#' x |>
#'   as.segmenter() |>
#'   model_args()
#'   
model_args <- function(object, ...) UseMethod("model_args")

#' @rdname model_args
#' @export
model_args.default <- function(object, ...) {
  object$model_fn_args
}

#' Convert, retrieve, or verify a model object
#' @param object A [tidycpt-class] object, typically returned by [segment()]
#' @param ... currently ignored
#' @details
#' [tidycpt-class] objects have a `model` component.
#' The functions documented here are convenience utility functions
#' for working with the `model` components. 
#' [as.model()] is especially useful in pipelines to avoid having to use 
#' the `$` or `[` notation for subsetting.
#' 
#' When applied to a [tidycpt-class] object, [as.model()] simply returns the 
#' `model` component of that object.
#' However, when applied to a `segmenter` object, [as.model()] attempts to 
#' converts that object into a [mod_cpt] model object.
#' @family tidycpt-generics
#' @return 
#'   - [as.model()] returns a [mod_cpt] model object
#' @export
as.model <- function(object, ...) UseMethod("as.model")

#' @rdname as.model
#' @export
#' @examples
#' # Segment a time series using PELT
#' x <- segment(CET, method = "pelt")
#' 
#' # Retrieve the model component
#' x |> 
#'   as.model()
#' 
#' # Explicitly convert the segmenter to a model
#' x |>
#'   as.segmenter() |>
#'   as.model()
#' 
#' # Is that model valid? 
#' x |>
#'   as.model() |>
#'   is_model()
#'   
as.model.default <- function(object, ...) {
  f <- whomademe(object)
  args <- c(list(x = as.ts(object), tau = changepoints(object)), model_args(object), list(...))
  do.call(f, args)
}

#' Modified Bayesian Information Criterion
#' 
#' @description
#' Generic function to compute the Modified Bayesian Information Criterion for a
#' changepoint detection model. 
#' @inheritParams stats::logLik
#' @return A `double` vector of length 1
#' @family penalty-functions
#' @export
#' @seealso [stats::BIC()]
MBIC <- function(object, ...) UseMethod("MBIC")

#' @rdname MBIC
#' @export
MBIC.default <- function(object, ...) {
  MBIC(logLik(object))
}

#' Maximum Descriptive Length
#' 
#' @description
#' Generic function to compute the Maximum Descriptive Length for a
#' changepoint detection model. 
#' @details
#' \deqn{
#'    P_{MDL}(\tau) = \frac{a(\theta_\tau)}{2} \cdot 
#'      \sum_{j=0}^m \log{\left(\tau_j - \tau_{j-1} \right)} + 2 \ln{m} + \sum_{j=2}^m \ln{\tau_j} + 
#'      \left( 2 + b(\theta_\tau) \right) \ln{n} 
#'  }
#'  where \eqn{a(\theta)} is the number of parameters in \eqn{\theta} that are 
#'  fit in each region, and \eqn{b(\theta)} is the number of parameters 
#'  fit to the model as a whole. 
#' @return A `double` vector of length 1
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
#' @description
#' Generic function to compute the Bayesian Maximum Descriptive Length for a
#' changepoint detection model. 
#' 
#' @details
#' Currently, the BMDL function is only defined for the NHPP model 
#' (see [fit_nhpp()]).
#' Given a changepoint set \eqn{\tau}, the BMDL is: 
#'  \deqn{
#'    BMDL(\tau, NHPP(y | \hat{\theta}_\tau) = 
#'    P_{MDL}(\tau) - 2 \ln{ L_{NHPP}(y | \hat{\theta}_\tau) } 
#'    - 2 \ln{ g(\hat{\theta}_\tau) }
#'  }
#' where \eqn{P_{MDL}(\tau)} is the [MDL()] penalty. 
#' @return A `double` vector of length 1
#' @inheritParams stats::logLik
#' @family penalty-functions
#' @export
BMDL <- function(object, ...) UseMethod("BMDL")

#' @rdname BMDL
#' @export
#' @examples
#' # Compute the BMDL
#' BMDL(fit_nhpp(DataCPSim, tau = NULL))
#' BMDL(fit_nhpp(DataCPSim, tau = c(365, 830)))
BMDL.default <- function(object, ...) {
  BMDL(logLik(object))
}

#' Evaluate candidate changepoints sets
#' @param x An object to evaluate
#' @param ... arguments passed to methods
#' @export
#' @returns A [tibble::tbl_df]
#' @keywords internal
#' @export
evaluate_cpts <- function(x, ...) UseMethod("evaluate_cpts")

#' Diagnose the fit of a segmented time series
#' @param x A [tidycpt-class] object, or a `model` or `segmenter`
#' @param ... currently ignored
#' @description
#' 
#' Depending on the input, this function returns a diagnostic plot. 
#' 
#' @returns A [ggplot2::ggplot()] object
#' @family tidycpt-generics
#' @export
diagnose <- function(x, ...) UseMethod("diagnose")