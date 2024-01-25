#' Broom compatibility layer
#' @export
#' @seealso [broom::tidy()]
tidy <- broom::tidy

#' @rdname tidy
#' @export
#' @seealso [broom::augment()]
augment <- broom::augment

#' @rdname tidy
#' @export
#' @seealso [broom::glance()]
glance <- broom::glance
