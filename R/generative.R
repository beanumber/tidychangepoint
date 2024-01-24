

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




#' generate_mean_cumulative
#' Genera el texto correspondiente a la variable m para el archivo .txt para JAGS
#'
#' @param n_puntos_cambio numero de puntos de cambio
#' @param mas_derecha es una variable que fue utilizada para que
#'  pudiera ejecutarse en R y en JAGS; toma los valores "" (vacío) y "+".
#'
#' @return Regresa el texto correspondiente a la variable m
#' @export
#'
#' @examples
#' \dontrun{
#' mejor_cp <- historia_mejores[which(
#'   Bayesaian_MDL_k_cp(
#'     historia_mejores, x, param$rf_type, param$initial_val_optim,
#'     param$mat_low_upp, param$vec_dist_a_priori, param$mat_phi
#'   ) == min(Bayesaian_MDL_k_cp(
#'     historia_mejores, x, param$rf_type, param$initial_val_optim,
#'     param$mat_low_upp, param$vec_dist_a_priori, param$mat_phi
#'   ))
#' ), ]
#' }
#' d <- exceedances(lista_AG$data)
#' tau <- cpt_best(lista_AG)
#' theta <- cpt_best_params(lista_AG)
#' alpha <- theta$alpha
#' sigma <- theta$beta
#' f <- generate_mean_cumulative(length(tau))
#' f(1:length(d), d, tau, alpha, sigma)
generate_mean_cumulative <- function(n_puntos_cambio, mas_derecha = "", destdir = tempdir()) {
  # PRIMER RENGLON
  j_renglon <- 1
  # texto_m <- paste0("m[i] <- ",print_step("i",j_renglon,T),"*",print_pow_d("i",j_renglon),mas_derecha)
  texto_m <- paste0(print_step("i", j_renglon, T), "*", print_pow_d("i", j_renglon), "+", "\n", mas_derecha)
  # SEGUNDO RENGLON
  j_renglon <- 2
  ini_renglon <- paste0("+ ", print_step("i", j_renglon - 1), "*", print_step("i", j_renglon, T), "*(")
  pre_med_ren <- print_pow_d("i", j_renglon)
  med_renglon <- paste0("+", print_pow_tau(j_renglon - 1, j_renglon - 1), "-", print_pow_tau(j_renglon - 1, j_renglon))
  fin_renglon <- paste0(")", "+", "\n", mas_derecha)
  ser <- paste0(ini_renglon, pre_med_ren, med_renglon, fin_renglon)
  texto_m <- paste0(texto_m, ser)
  # TERCER RENGLON Y OTROS
  for (j_renglon in 3:(n_puntos_cambio + 1)) {
    if (j_renglon < (n_puntos_cambio + 1)) ini_renglon <- paste0("+ ", print_step("i", j_renglon - 1), "*", print_step("i", j_renglon, T), "*(")
    if (j_renglon == (n_puntos_cambio + 1)) ini_renglon <- paste0("+ ", print_step("i", j_renglon - 1), "*(")
    pre_med_ren <- print_pow_d("i", j_renglon)
    med_renglon <- paste0("+", print_pow_tau(j_renglon - 1, j_renglon - 1), med_renglon, "-", print_pow_tau(j_renglon - 1, j_renglon))
    fin_renglon <- paste0(")", "+", "\n", mas_derecha)
    texto_m <- paste0(texto_m, ini_renglon, pre_med_ren, med_renglon, fin_renglon)
  }
  if (mas_derecha == "") texto_m <- paste0(texto_m, "0")
  # estas tres lineas son nuevas
  texto_m <- paste0("funcion_media_acumulada <- function (i, d, tau, alpha, sigma) {", texto_m)
  texto_m <- paste0(texto_m, "}")
  
  file_path <- fs::path(destdir, paste0("funcion_media_acumulada.R"))
  cat(texto_m, file = file_path)
  
  f <- eval(parse(text = texto_m))
  return(f)
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
#' genera_insumos_bloque_sin_theta(chromosome_best(lista_AG), lista_AG$data)
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
#' split_by_tau(lista_AG$data, cpt_best(lista_AG))

split_by_tau <- function(x, tau) {
  idx <- cut(
    1:length(x), 
    breaks = c(0, tau, length(x)), 
    include.lowest = TRUE, 
    right = FALSE
  )
  split(x, idx)
}
