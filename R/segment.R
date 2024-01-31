#' Segment a time series using Genetic BMDL heuristic
#' @export
#' @examples
#' mod_null <- segment(DataCPSim)
#' augment(mod_null)
#' tidy(mod_null)
#' glance(mod_null)

segment <- function(x, method = "null", ...) UseMethod("segment")

#' @rdname segment
#' @export

segment.numeric <- function(x, method = "null", ...) {
  if (!stats::is.ts(stats::as.ts(x))) {
    stop("x is not coercible into a ts object.")
  }
  segment(as.ts(x))
}

#' @rdname segment
#' @export

segment.ts <- function(x, method = "null", ...) {
  if (method == "cpt-pelt") {
    return(changepoint::cpt.meanvar(data = x, method = "PELT"))
  }
  
  mod_null <- lm(y ~ 1, data = data.frame(y = x))
  class(mod_null) <- c("cpt_lm", class(mod_null))
  return(mod_null)
}