#' Segment a time series using the PELT algorithm
#' 
#' @description
#' Segmenting functions for the PELT algorithm
#' 
#' @param x A time series
#' @param model_fn A `character` or `name` coercible into a [fun_cpt] function. 
#' See, for example, [fit_meanshift_norm()]. The default is [fit_meanvar()].
#' @param ... arguments passed to [changepoint::cpt.meanvar()] or 
#' [changepoint::cpt.mean()]
#' @details
#' This function wraps either [changepoint::cpt.meanvar()] or 
#' [changepoint::cpt.mean()].
#' 
#' @returns A `cpt` object returned by [changepoint::cpt.meanvar()] or 
#' [changepoint::cpt.mean()]
#' @export
#' @examples
#' # Segment a time series using PELT
#' res <- segment_pelt(DataCPSim)
#' res
#' str(res)
#' 
#' # Segment as time series while specifying a penalty function
#' segment_pelt(DataCPSim, penalty = "BIC")
#' 
#' # Segment a time series while specifying a meanshift normal model
#' segment_pelt(DataCPSim, model_fn = fit_meanshift_norm, penalty = "BIC")
#' 
segment_pelt <- function(x, model_fn = fit_meanvar, ...) {
  if (!inherits(model_fn, "fun_cpt")) {
    model_fn <- fun_cpt(model_fn)
  }
  if (!inherits(model_fn, "fun_cpt")) {
    stop("model_fn must be coercible into a fun_cpt")
  }

  if (identical(model_fn, fit_meanshift_norm)) {
    out <- x |>
      changepoint::cpt.mean(method = "PELT", test.stat = "Normal", class = TRUE, ...)
  } else {
    out <- x |>
      changepoint::cpt.meanvar(method = "PELT", test.stat = "Normal", class = TRUE, ...)
  }
  return(out)
}


