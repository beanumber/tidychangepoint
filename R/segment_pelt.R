#' Segment a time series using the PELT algorithm
#' 
#' @description
#' Segmenting functions for the PELT algorithm
#' 
#' @param x A time series
#' @param model_fn A `character` or `name` coercible into a [fun_cpt()] function. 
#' See, for example, [fit_meanshift_norm()].
#' @param penalty_fn A function that evaluates the changepoint set returned by
#' `model_fn`. We provide [AIC()], [BIC()], [MBIC()], [MDL()], and [BMDL()].
#' @param ... arguments passed to [changepoint::meanvar()]
#' @export
#' @examples
#' res <- segment_pelt(DataCPSim)
#' summary(res)
#' str(res)
#' plot(res)
#' segment_pelt(as.ts(CET))
#' segment_pelt(as.ts(CET), penalty = "BIC")
#' segment_pelt(as.ts(CET), model_fn = fit_meanvar, penalty = "BIC")

segment_pelt <- function(x, model_fn = fit_meanshift_norm, ...) {
  if (!inherits(model_fn, "fun_cpt")) {
    model_fn <- fun_cpt(model_fn)
  }
  if (!inherits(model_fn, "fun_cpt")) {
    stop("model_fn must be coercible into a fun_cpt")
  }

  if (identical(model_fn, fit_meanvar)) {
    out <- x |>
      changepoint::cpt.meanvar(method = "PELT", test.stat = "Normal", class = TRUE, ...)
  } else {
    out <- x |>
      changepoint::cpt.mean(method = "PELT", test.stat = "Normal", class = TRUE, ...)
  }
  return(out)
}


