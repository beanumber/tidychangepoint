#' Compatibility with changepoint package
#' @param lista_AG list of candidate changepoints returned by [AG_BMDL_r_paso()]
#' @return 
#' - For `cpt_candidates()`, a [tibble::tibble()] where each row represents a 
#' possible set of changepoints and a BMDL score. 
#' - For `cpt_best()`, a single vector of changepoints with the lowest BDML 
#' score.
#' - For `cpt_meanvar()`, a `ch`
#' @export
#' @examples
#' cpts_list <- cpt_candidates(lista_AG)
#' cpts <- cpt_meanvar(lista_AG)
#' class(cpts)
#' 
cpt_candidates <- function(lista_AG) {
  num_generations <- nrow(lista_AG$historia_mejores)
  max_size <- ncol(lista_AG$historia_mejores)
  cpt_list <- lista_AG$historia_mejores[, 3:max_size] |>
    apply(MARGIN = 1, list) |>
    lapply(unlist) |>
    lapply(FUN = function(x) x[x > 0])
  tibble::tibble(
    cpt = cpt_list,
    bmdl = lista_AG$vec_min_BMDL
  )
}

#' @rdname cpt_candidates
#' @export

cpt_best <- function(lista_AG) {
  lista_AG |>
    cpt_candidates() |>
    dplyr::arrange(bmdl) |>
    utils::head(1) |>
    dplyr::pull(cpt) |>
    unlist()
}

#' @rdname cpt_candidates
#' @importClassesFrom changepoint cpt
#' @export

cpt_meanvar <- function(lista_AG) {
  x <- methods::new(
    "cpt", 
    cpttype = "mean and variance",
    data.set = stats::as.ts(lista_AG$data), 
    test.stat = "Normal",
    cpts = cpt_best(lista_AG),
    param.est = list(
      mean = mean(lista_AG$data),
      variance = stats::var(lista_AG$data)
    )
  )
  return(x)
}
