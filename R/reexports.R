#' Objects exported from other packages
#' @export
#' @importFrom broom tidy
broom::tidy

#' @export
#' @importFrom broom augment
broom::augment

#' @export
#' @importFrom broom glance
broom::glance

#' @export
#' @importFrom vctrs vec_ptype2
vctrs::vec_ptype2

#' @export
#' @importFrom vctrs vec_cast
vctrs::vec_cast

#' @export
#' @importFrom xts as.xts
as.ts.xts <- xts:::as.ts.xts

#' @export
#' @importFrom zoo index
zoo::index
