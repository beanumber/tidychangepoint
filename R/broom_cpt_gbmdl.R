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

augment.cpt_gbmdl <- function(x, ...) {
  cpt <- cpt_best(x)
  data <- x$data
  n <- length(data)
  data |>
    tsibble::as_tsibble() |>
    dplyr::mutate(region = cut(
      index, 
      breaks = unique(c(0, cpt, n)), 
      include.lowest = TRUE, 
      right = FALSE)
    ) |>
  dplyr::group_by(region)
}

#' @rdname augment.cpt_gbmdl
#' @export

tidy.cpt_gbmdl <- function(x, ...) {
  augment(x) |>
    dplyr::ungroup() |>
    # why is this necessary????
    as.data.frame() |>
    dplyr::group_by(region) |>
    dplyr::summarize(
      num_obs = dplyr::n(),
      min = min(value),
      max = max(value),
      mean = mean(value),
      sd = sd(value),
      ... = ...
    )
}

#' @rdname augment.cpt_gbmdl
#' @export

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

#' @rdname augment.cpt_gbmdl
#' @export
#' @examples
#' cpts <- lista_AG
#' class(cpts) <- c("cpt_bmdl", class(cpts))
#' as.ts(cpts)
as.ts.cpt_gbmdl <- function(x, ...) {
  as.ts(x$data)
}

#' @rdname augment.cpt_gbmdl
#' @export
changepoints.cpt_gbmdl <- function(x, ...) {
  cpt_best(x) |>
    as.integer()
}
