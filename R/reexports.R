#' Broom compatibility layer
#' @export
#' @importFrom broom tidy
#' @seealso [broom::tidy()]
broom::tidy

#' @rdname tidy
#' @export
#' @importFrom broom augment
#' @seealso [broom::augment()]
broom::augment

#' @rdname tidy
#' @export
#' @importFrom broom glance
#' @seealso [broom::glance()]
broom::glance

#' @rdname tidy
#' @export
#' @importFrom stats as.ts
#' @seealso [stats::as.ts()]
stats::as.ts

#' @rdname tidy
#' @export
#' @importFrom stats logLik
#' @seealso [stats::logLik()]
stats::logLik

#' @rdname tidy
#' @export
#' @importFrom stats AIC
#' @seealso [stats::AIC()]
stats::AIC

#' @rdname tidy
#' @export
#' @importFrom stats BIC
#' @seealso [stats::BIC()]
stats::BIC

#' @rdname tidy
#' @export
#' @importFrom stats nobs
#' @seealso [stats::nobs()]
stats::nobs
