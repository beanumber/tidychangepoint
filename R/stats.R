#' Generic methods for classes from [stats]
#' 




#' @rdname logLik.ts
#' @export
exceedances.ts <- function(x, ...) {
  exceedances(as.double(x))
}

#' @rdname logLik.ts
#' @param x A numeric vector
#' @export
exceedances.double <- function(x, ...) {
  which(x > mean(x, na.rm = TRUE, ...))
}

#' @rdname logLik.ts
#' @export
#' @examples
#' MDL(logLik(as.ts(CET), loc.ind = tau2binary(123, n = length(CET))))
MDL.logLik <- function(object, ...) {
  tau <- attr(object, "tau")
  n <- nobs(object)
  penalty_mdl(pad_tau(tau, n)) - 2 * object |>
    as.double()
}

#' @rdname logLik.ts
#' @references Zhang and Seigmmund (2007) for MBIC: \doi{10.1111/j.1541-0420.2006.00662.x}
#' @export
MBIC.logLik <- function(object, ...) {
  tau <- attr(object, "tau")
  m <- length(tau)
  if (m == 0) {
    penalty <- 0
  } else {
    n <- nobs(x)
    padded_tau <- pad_tau(tau, n)
    penalty <- 3 * m * log(n) + sum(log(diff(padded_tau) / n)) 
  }
  penalty - 2 * object |>
    as.double()
}