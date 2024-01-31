#' Broom compatibility layer for changepoint
#' @export
#' @examples
#' cpts <- lista_AG
#' class(cpts) <- c("cpt_gbmdl", class(cpts))
#' y <- augment(cpts)
#' class(y)
#' y
#' tidy(cpts)
#' glance(cpts)

glance.cpt_gbmdl <- function(x, ...) {
  tibble::tibble(
    pkg = "tidychangepoint",
    version = packageVersion("BayesianMDLGA"),
    algorithm = "GeneticBMDL",
    test_stat = cpt_best_bmdl(x),
    BMDL = cpt_best_bmdl(x),
    num_cpts = length(cpt_best(x)),
  )
}

#' @rdname glance.cpt_gbmdl
#' @export
#' @examples
#' cpts <- lista_AG
#' class(cpts) <- c("cpt_bmdl", class(cpts))
#' as.ts(cpts)
as.ts.cpt_gbmdl <- function(x, ...) {
  as.ts(x$data)
}

#' @rdname glance.cpt_gbmdl
#' @export
changepoints.cpt_gbmdl <- function(x, ...) {
  cpt_best(x) |>
    as.integer()
}
