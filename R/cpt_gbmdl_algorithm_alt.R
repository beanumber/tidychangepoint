#' Alternate implementation of Genetic BMDL algorithm
#' @rdname segment_gbmdl
#' @export
#' @examples
#' \dontrun{
#' segment_gbmdl_alt(DataCPSim)
#' }

segment_gbmdl_alt <- function(x, destdir = tempdir(), show_progress_bar = TRUE, write_rda = FALSE) {
  # lista_AG contiene los resultados del algoritmo genético
  obj <- new_cpt_gbmdl(x, generation_size = 50, num_generations = 50)
  
  
  pb <- utils::txtProgressBar(min = 1, max = num_generations(obj), style = 3, width = 60)
  
  basket <- sim_k_cp_BMDL(x, generation_size = generation_size(obj), max_num_cp = 20) |>
    mat_cp_2_tbl() |>
    dplyr::pull(tau)
  keepers <- list()
  for (i in 1:num_generations(obj)) {
    these_bmdls <- basket |>
      purrr::map_dbl(bmdl, x = x)
      
    best_idx <- which.min(these_bmdls)
    
    keepers[[i]] <- basket[[best_idx]]
    
    if (show_progress_bar) {
      utils::setTxtProgressBar(pb, i)
    }
    
#    plot_evolution(obj, i)
#    plot_cpt_repeated(obj, i)
    
    basket <- evolve_alt(x, basket)
  }
  close(pb)
  
  obj$keepers <- keepers
  obj$min_bmdls <- keepers |>
    purrr::map_dbl(bmdl, x = x)
  
  # Write data object
  if (write_rda) {
    write_cpt_gbmdl(obj)
  }
  return(obj)
}


#' @rdname segment_gbmdl
#' @export
#' @examples
#' \dontrun{
#' mat_cp <- lista_AG$segmenter$lista_AG_BMDL$mat_cp
#' 
#' Bayesaian_MDL_k_cp(mat_cp, exceedances(DataCPSim))
#' basket <- sim_k_cp_BMDL(DataCPSim, 50, max_num_cp = 20) |>
#'   mat_cp_2_tbl() |>
#'   dplyr::pull(tau)
#' evolve_alt(DataCPSim, basket) |>
#'   lapply(bmdl, x = DataCPSim)
#'   }

evolve_alt <- function(x, cpt_list) {
  k <- length(cpt_list)
  generation <- cpt_list |>
    tibble::enframe(value = "tau") |>
    dplyr::mutate(
      bmdl = purrr::map_dbl(tau, bmdl, x = x),
      rank = dplyr::min_rank(-bmdl),
      papa_idx = sample.int(k, prob = rank, replace = TRUE),
      # need to correct if papa == mama
      mama_idx = sample.int(k, prob = rank, replace = TRUE),
      bad = papa_idx == mama_idx,
      mama_idx = ifelse(bad, sample.int(k, size = 1, replace = TRUE, prob = rank), mama_idx)
    )
  children <- purrr::map2(
    generation$tau[generation$papa_idx], 
    generation$tau[generation$mama_idx],
    function(.x, .y) sort(unique(c(.x, .y)))
  )
  children |>
    purrr::map(drop_cpts)  # need to add mutation step
}

#' @export

drop_cpts <- function(x, prob_stay = 0.5) {
  x[as.logical(stats::rbinom(n = length(x), size = 1, prob = prob_stay))]
}
