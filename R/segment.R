#' Segment a time series using a variety of algorithms
#' @param x a numeric vector coercible into a [stats::ts] object
#' @param method a character string indicating the algorithm to use
#' @param ... arguments passed to methods
#' @return an object of class `changepoint::cpt`, [stats::lm], or [cpt_gbmdl]
#' @seealso [changepoint::cpt.meanvar()]
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
  segment(as.ts(x), method = method, ... = ...)
}

#' @rdname segment
#' @export
#' @examples
#' segment(DataCPSim, method = "cpt-pelt")
#' segment(DataCPSim, method = "cpt-pelt", penalty = "AIC")
#' segment(DataCPSim, method = "cpt-binseg", penalty = "AIC")
#' segment(DataCPSim, method = "cpt-segneigh", penalty = "BIC")
#' segment(DataCPSim, method = "cpt-manual", cpts = c(826))
#' segment(DataCPSim, method = "cpt-manual", cpts = c(365, 826))
#' 

segment.ts <- function(x, method = "null", ...) {
  message(paste("method:", method))
  n <- length(x)
  ds <- data.frame(y = x, t = 1:n)
  if (method == "cpt-pelt") {
    return(changepoint::cpt.meanvar(data = x, method = "PELT", ...))
  }
  if (method == "cpt-binseg") {
    return(changepoint::cpt.meanvar(data = x, method = "BinSeg", ...))
  }
  if (method == "cpt-segneigh") {
    return(changepoint::cpt.meanvar(data = x, method = "SegNeigh", ...))
  }
  if (method == "cpt-manual") {
    message("\nSegmenting using manually input changepoints...")
    args <- list(...)
    cpts <- args[["cpts"]]
    terms <- paste("(t > ", cpts, ")") |>
      paste(collapse = "+")
    form <- stats::as.formula(paste("x ~ ", terms))
    mod <- stats::lm(form, data = ds)
    class(mod) <- c("cpt_lm", class(mod))
    return(mod)
  }
  
  mod_null <- lm(y ~ 1, data = ds)
  class(mod_null) <- c("cpt_lm", class(mod_null))
  return(mod_null)
}