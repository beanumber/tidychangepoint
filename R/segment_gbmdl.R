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
segment_gbmdl <- function(x, param, destdir = tempdir(), show_progress_bar = TRUE, write_rda = FALSE) {
  # lista_AG contiene los resultados del algoritmo genético
  obj <- new_cpt_gbmdl(x, param = param)
  
  pb <- utils::txtProgressBar(min = 1, max = param$r, style = 3, width = 60)
  graphics::par(mfrow = c(2, 1), mar = c(1, 4, 2, 2))
  for (i in 1:param$r) {
    # Hacemos un paso del AG con el mat_cp anterior
    vec_BMDL_k_cp <- Bayesaian_MDL_k_cp(obj$mat_cp, exceedances(x))
    obj$mat_cp <- evolve(exceedances(as.ts(obj)), obj$mat_cp)
    
    # Obtenemos el índice del mínimo
    (i_min_BMDL <- which.min(vec_BMDL_k_cp))
    # Guardamos el cromosoma mínimo
    obj$historia_mejores[i, ] <- obj$mat_cp[i_min_BMDL, ]
    # Guardamos el BMDL del cromosoma mínimo
    obj$vec_min_BMDL[i] <- vec_BMDL_k_cp[i_min_BMDL]
    # Imprimimos el porcentaje de progreso
    
    if (show_progress_bar) {
      utils::setTxtProgressBar(pb, i)
    }

    plot_evolution(obj, i)
    plot_cpt_repeated(obj, i)
  }
  close(pb)
  graphics::par(mfrow = c(1, 1))

  # Write data object
  if (write_rda) {
    write_cpt_gbmdl(obj)
  }
  return(obj)
}

#' @export
#' @examples
#' mat_cp <- lista_AG$segmenter$mat_cp
#' segment_gbmdl_1(exceedances(DataCPSim), mat_cp)
#' 

evolve <- function(x, mat_cp) {
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
  return(mat_cp)
}

