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
