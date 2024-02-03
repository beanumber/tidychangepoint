

#' Genera los textos para los pdf y RData de las n distribuciones
#' @param param description
#' @export
fun_n_genera_texto_dist <- function(param) {
  texto_n_dist <- NULL
  for (i in 1:length(param$vec_dist_a_priori)) {
    dist <- param$vec_dist_a_priori[i]
    parametros_dist <- param$mat_phi[i, ]
    texto_n_dist <- c(texto_n_dist, fun_1_genera_texto_dist(dist, parametros_dist))
  }
  texto_n_dist <- paste(texto_n_dist, collapse = "_")
  return(texto_n_dist)
}

#' Genera los textos para los pdf y RData de una distribución
#' @rdname fun_n_genera_texto_dist
#' @param dist description
#' @param parametros_dist description
#' @export
fun_1_genera_texto_dist <- function(dist, parametros_dist) {
  if (any(dist == c("Gamma", "Unif"))) {
    texto <- paste0(dist, "(", parametros_dist[1], ",", parametros_dist[2], ")")
  } else {
    for (i in 1:10) print("no se tiene esta distribución; fun_1_genera_texto_dist")
  }
  return(texto)
}




#' Agrupamos los insumos por bloques
#'
#' @param cp vector de tamaño max_num_cp con entradas m, tau_0=1 , ...,
#'   tau_{m+1}, 0, ..., 0
#' @param x datos de rebases
#'
#' @return regresa una lista con una matriz de 5 columnas la cual tiene
#'         los el tau_{i-1}, tau_i, theta_1, theta_2 y theta_3, y también
#'         una lista con los días de cada régimen
#' @export
#' @examples
#' genera_insumos_bloque_sin_theta(chromosome_best(lista_AG$segmenter), as.ts(lista_AG))
#' 
genera_insumos_bloque_sin_theta <- function(cp, x) {
  (cp_corto_cero <- c(0, cp[3:(cp[1] + 3)]))
  # Primero calculamos la matriz con taus
  # mat_tau_theta <- cbind(matrix(c(cp_corto_cero[-length(cp_corto_cero)],cp_corto_cero[-1]),ncol = 2),t(theta_mat)) # ANTES
  mat_tau <- matrix(c(cp_corto_cero[-length(cp_corto_cero)], cp_corto_cero[-1]), ncol = 2)
  # Ahora hacemos una lista de días
  # Para el resto de la log-veros, hacemos una matriz de dos columnas con los puntos de cambio, para definir los intervalos de regímenes
  mat_intervalos_cp <- matrix(c(cp_corto_cero[-length(cp_corto_cero)], cp_corto_cero[-1]), ncol = 2)
  # lista con los días de excedencias en cada regimen
  (lista_dias_regimen <- apply(mat_intervalos_cp, 1, function(y) {
    x[x > y[1] & x <= y[2]]
  }))
  lista_insumos_bloque <- list(mat_tau = mat_tau, lista_dias_regimen = lista_dias_regimen)
  return(lista_insumos_bloque)
}

#' @rdname genera_insumos_bloque_sin_theta
#' @export
#' @examples
#' split_by_tau(as.ts(lista_AG), changepoints(lista_AG))

split_by_tau <- function(x, tau) {
  idx <- cut_inclusive(1:length(x), pad_tau(tau, length(x)))
  split(x, idx)
}
