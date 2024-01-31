#' Broom compatibility layer
#' @export
#' @importFrom broom tidy
#' @seealso [broom::tidy()]
tidy <- broom::tidy

#' @rdname tidy
#' @export
#' @importFrom broom augment
#' @seealso [broom::augment()]
augment <- broom::augment

#' @rdname tidy
#' @export
#' @importFrom broom glance
#' @seealso [broom::glance()]
glance <- broom::glance
