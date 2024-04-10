#' Compatibility layer for changepoint
#' @name cpt-generics
#' @param x A `cpt` object returned by [changepoint::cpt.meanvar()]
#' @param ... arguments passed to methods
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "pelt")
#' class(cpts)
#' y <- augment(cpts)
#' class(y)
#' y
#' tidy(cpts)
#' glance(cpts)

glance.cpt <- function(x, ...) {
  tibble::tibble(
    pkg = "changepoint",
    version = package_version(x@version),
    algorithm = x@method,
    params = list(params(x)),
    num_cpts = length(changepoints(x)),
    model = model_name(x),
    criteria = names(fitness(x)),
    fitness = fitness(x)
  )
}

#' @rdname cpt-generics
#' @export
params.cpt <- function(x, ...) {
  out <- list(
    test_stat = x@test.stat,
    penalty = x@pen.value,
    num_cpts_max = x@ncpts.max,
    min_seg_length = x@minseglen
  )
  # hack
  names(out)[2] <- x@pen.type
  out
}

#' @rdname cpt-generics
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "pelt")
#' as.ts(cpts)
#' 
as.ts.cpt <- function(x, ...) {
  as.ts(x@data.set)
}

#' @rdname cpt-generics
#' @param object A `cpt` object.
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "pelt")
#' nobs(cpts)
#' 
nobs.cpt <- function(object, ...) {
  length(as.ts(object@data.set))
}

#' @rdname cpt-generics
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "pelt", penalty = "BIC")
#' logLik(cpts$segmenter)
#' cpts <- segment(DataCPSim, method = "pelt", penalty = "AIC")
#' logLik(cpts$segmenter)
#' 
logLik.cpt <- function(object, ...) {
#  message("intercepting...")
  y <- changepoint::likelihood(object) |>
    suppressWarnings()
  ll <- -y[1] / 2
  attr(ll, "df") <- length(object@cpts)
  attr(ll, "nobs") <- nobs(object)
  attr(ll, "tau") <- changepoints(object)
  attr(ll, "real_params_estimated") <- (length(changepoints(object)) + 1) * 2
  class(ll) <- "logLik"
  return(ll)
}

#' @rdname changepoints
#' @export
#' @examples
#' cpts <- segment(DataCPSim, method = "pelt")
#' changepoints(cpts)
#' 
changepoints.cpt <- function(x, ...) {
  changepoint::cpts(x) |>
    as.integer()
}

#' @rdname fitness
#' @export
#' @examples
#' x <- segment(DataCPSim, method = "pelt")
#' fitness(x)
#' 
fitness.cpt <- function(object, ...) {
  out <- object@pen.value
  names(out) <- object@pen.type
  out
}

#' @rdname model_name
#' @export
model_name.cpt <- function(object, ...) {
  if (object@cpttype == "mean and variance") {
    return("meanvar")
  } else {
    return("meanshift")
  }
}
