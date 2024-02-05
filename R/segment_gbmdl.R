#' Algoritmo genético de Bayesian MDL a un paso
#'
#' @param x a vector that can be coerced into a `ts`
#' @param param description. See [param].
#'
#' @return A `cpt_gbmdl` object
#' @param show_progress_bar show the progress bar?
#' @export
#' @examples
#' \dontrun{
#' x <- segment_gbmdl(DataCPSim, param)
#' y <- segment_gbmdl(rlnorm_ts_1, param)
#' }
#' 
segment_gbmdl <- function(x, param, destdir = tempdir(), show_progress_bar = TRUE) {
  # lista_AG contiene los resultados del algoritmo genético
  lista_AG <- new_cpt_gbmdl(x, param = param)
  # El siguiete for va sobre en cada paso haciendo una nueva generación
  vec_para_for <- 1:param$r
  pb <- utils::txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  graphics::par(mfrow = c(2, 1), mar = c(1, 4, 2, 2))
  for (i in vec_para_for) {
    # Hacemos un paso del AG con el mat_cp anterior
    lista_AG$lista_AG_BMDL <- segment_gbmdl_1(
      exceedances(lista_AG$data), 
      lista_AG$lista_AG_BMDL$mat_cp
    )
    # Obtenemos el índice del mínimo
    (i_min_BMDL <- which.min(lista_AG$lista_AG_BMDL$vec_BMDL_k_cp))
    # Guardamos el cromosoma mínimo
    lista_AG$historia_mejores[i, ] <- lista_AG$lista_AG_BMDL$mat_cp[i_min_BMDL, ]
    # Guardamos el BMDL del cromosoma mínimo
    lista_AG$vec_min_BMDL[i] <- lista_AG$lista_AG_BMDL$vec_BMDL_k_cp[i_min_BMDL]
    # Imprimimos el porcentaje de progreso
    
    if (show_progress_bar) {
      utils::setTxtProgressBar(pb, i)
    }
    # # Graficamos el valor de BMDL
    # plot(lista_AG$vec_min_BMDL,
    #      xlim = c(1,param$r),type="l",col="blue",ylab="BMDL",xlab="Generation",
    #      main=paste0("AG rate ",param$rf_type," and priori ",paste(param$vec_dist_a_priori,collapse = "-")))
    plot_evolution(lista_AG, i)
    # Graficamos los puntos de cambio que más se repitieron
    plot_cpt_repeated(lista_AG, i)
  }
  close(pb)
  graphics::par(mfrow = c(1, 1))
  cat(" \n")
  
  # Write data object
  write_cpt_gbmdl(lista_AG)
  return(lista_AG)
}

#' @export
#' @examples
#' mat_cp <- lista_AG$segmenter$lista_AG_BMDL$mat_cp
#' segment_gbmdl_1(exceedances(DataCPSim), mat_cp)
#' 

segment_gbmdl_1 <- function(x, mat_cp) {
  # 1. Evaluación de sus calificaciones
  vec_BMDL_k_cp <- Bayesaian_MDL_k_cp(mat_cp, x)
  # 2. Encontrar sus probabilidades
  vec_probs <- probs_vec_MDL(vec_BMDL_k_cp)
  # 3. Seleccionar dos padres
  mat_padres <- selec_k_pares_de_padres(vec_probs)
  # 4. Juntar sus puntos de cambio
  mat_cp <- junta_k_puntos_cambio(mat_padres, mat_cp)
  # 5. Volados para quitar puntos de cambio
  mat_cp <- mata_k_tau_volado(mat_cp)
  # 6. Mutaciones puntos de cambio
  mat_cp <- muta_k_cp_BMDL(mat_cp, x, param)
  # (mat_cp <- muta_k_cp(mat_cp,param)) # antes
  
  # POR AHORA QUITE LA GENERACIÓN DE NUEVOS PUNTOS DE CAMBIO
  # 7. Genera nuevos puntos de cambio
  # mat_cp <- muta_k_nuevos(mat_cp, param$max_num_cp, N, param$p_m)
  
  # 8. Regresa el resultado
  return(list(mat_cp = mat_cp, vec_BMDL_k_cp = vec_BMDL_k_cp))
}


#' @export
#' @examples
#' mat_cp <- lista_AG$segmenter$lista_AG_BMDL$mat_cp
#' 
#' Bayesaian_MDL_k_cp(mat_cp, exceedances(DataCPSim))
#' segment_gbmdl_1_alt(DataCPSim, mat_cp)$bmdl

segment_gbmdl_1_alt <- function(x, mat_cp) {
  mat_cp_2_tbl(mat_cp) |>
    dplyr::mutate(bmdl = purrr::map_dbl(tau, bmdl, x = x))
}
