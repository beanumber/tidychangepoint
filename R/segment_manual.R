#' Manually segment a times series
#' @inheritParams fit_meanshift_norm
#' @export
#' @examples
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
    fitness = c(BIC = BIC(m))
  )
}
