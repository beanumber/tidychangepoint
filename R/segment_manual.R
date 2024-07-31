#' Manually segment a time series
#' @description
#' Segment a time series by manually inputting the changepoint set
#' 
#' @inheritParams fit_meanshift_norm
#' @param ... arguments passed to [seg_cpt]
#' @export
#' @details
#' Sometimes you want to see how a manually input set of changepoints performs. 
#' This function takes a time series and a changepoint detection set as inputs
#' and returns a [seg_cpt] object representing the segmenter. 
#' Note that by default [fit_meanshift_norm()] is used to fit the model and
#' [BIC()] is used as the penalized objective function. 
#' @returns A [seg_cpt] object
#' @examples
#' # Segment a time series manually
#' segment_manual(CET, tau = c(84, 330))
#' segment_manual(CET, tau = NULL)
#' 
segment_manual <- function(x, tau, ...) {
  m <- fit_meanshift_norm(x, tau)
  seg_cpt(
    x, 
    pkg = "tidychangepoint", 
    algorithm = "manual", 
    changepoints = tau,
    fitness = c(BIC = BIC(m)),
    ... = ...
  )
}
