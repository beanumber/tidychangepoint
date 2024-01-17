#' Grafica los n puntos de cambio más repetidos sobre el plot.stepfun de los datos
#'
#' @param cpt_list description
#' @return guarda un pdf con la gráfica
#' @export
#' @examples
#' plot_cpt_repetidos(lista_AG)
#' 
plot_cpt_repetidos <- function(cpt_list, destdir = tempdir(), pdf = FALSE) {
  # Obtenemos cuantos de los cp mejores se graficarán
  n_cp_mas_repetidos <- cpt_list$param$cuantos_mejores_cp_graf
  historia_mejores_sin_0_1_N <- cpt_list$historia_mejores[, -1:-2]
  historia_mejores_sin_0_1_N <- historia_mejores_sin_0_1_N[historia_mejores_sin_0_1_N > 0 & historia_mejores_sin_0_1_N < max(cpt_list$data)]
  cp_mas_repetidos <- rev(sort(table(historia_mejores_sin_0_1_N)))[1:n_cp_mas_repetidos]
  stats::plot.stepfun(
    cpt_list$data,
    col.vert = "gray20", 
    main = paste0("Los ", n_cp_mas_repetidos, " CP mas repetidos ", fun_n_genera_texto_dist(cpt_list$param)),
    xlim = range(cpt_list$data)
  )
  graphics::abline(v = as.numeric(names(cp_mas_repetidos)), col = "blue")

  nombre_pdf <- paste0(
    "Fig_CP_repetidos_", cpt_list$param$nombre_datos, "_rf_",
    cpt_list$param$rf_type, "_", fun_n_genera_texto_dist(cpt_list$param), "_r",
    cpt_list$param$r, "_k",
    cpt_list$param$k, 
    cpt_best_bmdl_string(cpt_list), ".pdf"
  )
  
  path <- fs::path(destdir, nombre_pdf)
  
  if (pdf) {
    grDevices::dev.print(pdf, path, width = 16, height = 10)
  }
  message("Se guardo la imagen:\n", path, "\n")
}


#' Grafica escalonada para un proceso poisson
#'
#' @param x a time series vector
#' @export
#'
#' @examples
#' plot_exceedances(DataCPSim)
#' plot_exceedances(rlnorm_ts_1)
#' plot_exceedances(rlnorm_ts_2)
#' plot_exceedances(rlnorm_ts_3)
#'
plot_exceedances <- function(x, ...) {
  z <- exceedances(x)
  # Graficando
  plot(
    x = z, y = 1:length(z), type = "s",
    xlab = "Time units - t", ylab = "Number of exceedances at t",
    main = expression(bold(paste("(a) Confidence Interval for fitted m(t|", theta, ")"))), 
    ...
  )
}


#' Graficas de los intervalos y los datos
#'
#' @param lista_AG description
#' @param param description
#' @export
#' @examples
#' plot_confint(lista_AG)
#' 
plot_confint <- function(cpt_list) {
  tau <- cpt_best(cpt_list)
  theta <- cpt_best_params(cpt_list)
  
  sigma <- theta$beta
  alpha <- theta$alpha
  d <- 5
  
  # sink("funcion_media_acumulada.txt")
  gen_texto_m(length(tau), mas_derecha = "")
  # sink()
  
  plot_exceedances(cpt_list$data)
  
  tasa_NHPP <- funcion_media_acumulada(i = 2, cpt_list$exceedances, alpha, sigma, tau)
  upp_bond <- stats::qpois(.95, lambda = c(pow(10 / sigma[1], alpha[1]), tasa_NHPP))
  low_bond <- stats::qpois(.05, lambda = c(pow(10 / sigma[1], alpha[1]), tasa_NHPP))
  mean_NHPP <- c(pow(10 / sigma[1], alpha[1]), tasa_NHPP)
  graphics::lines(c(10, d), upp_bond, col = "blue", lwd = 2)
  graphics::lines(c(10, d), mean_NHPP, col = "red", lwd = 2)
  graphics::lines(c(10, d), low_bond, col = "blue", lwd = 2)
}


#' Graficas del AG con BMDL
#' @rdname grafica_datos_e_intervalos
#' @return una gráfica con :
#' 1 Datos reales
#' 2 Evolución del algoritmo genético
#' 3 Puntos de cambio que más se repitieron
#' 4 Number of change points in the best chromosomes
#'
#' @export
#' @examples
#' plot_BMDL(lista_AG)
#' 
plot_BMDL <- function(cpt_list, destdir = tempdir(), pdf = FALSE) {
  graphics::par(mfrow = c(2, 2), mar = c(2, 4, 2, 2))
  # 1.- Datos reales
  plot_confint(cpt_list)
  # 2.- Evolución del algoritmo genético
  plot_evolution(cpt_list)
  # 3.- Puntos de cambio que más se repitieron
  plot_cpt_repeated(cpt_list)
  # 4.- Number of change points in the best chromosomes
  plot_best_chromosome(cpt_list)

  graphics::par(mfrow = c(1, 1))
  
  nombre_pdf <- paste0(
    "Fig_4AGBMDL_", cpt_list$param$nombre_datos, "_rf_",
    cpt_list$param$rf_type, "_", fun_n_genera_texto_dist(cpt_list$param), "_r",
    cpt_list$param$r, "_k",
    cpt_list$param$k, 
    cpt_best_bmdl_string(cpt_list), ".pdf"
  )
  
  if (pdf) {
    grDevices::dev.print(pdf, fs::path(destdir, nombre_pdf), width = 16, height = 10)
  }
  message("Se guardo la imagen:\n", fs::path(destdir, nombre_pdf), "\n")
}

#' @rdname grafica_datos_e_intervalos
#' @export
#' @examples
#' plot_evolution(lista_AG)
#' plot_evolution(lista_AG, 5)
#' 
plot_evolution <- function(cpt_list, i = length(cpt_list$vec_min_BMDL)) {
  plot(
    cpt_list$vec_min_BMDL[1:i],
    xlim = c(1, cpt_list$param$r), 
    type = "l", 
    col = "blue", 
    ylab = "BMDL", 
    xlab = "Generation", 
    main = paste0(
      "AG rate ", cpt_list$param$rf_type, " and priori ", 
#      paste(cpt_list$param$vec_dist_a_priori, collapse = "-")
      fun_n_genera_texto_dist(cpt_list$param)
    )
  )
}

#' @rdname grafica_datos_e_intervalos
#' @export
#' @examples
#' plot_cpt_repeated(lista_AG)
#' plot_cpt_repeated(lista_AG, 5)
plot_cpt_repeated <- function(cpt_list, i = nrow(cpt_list$historia_mejores)) {
  historia_mejores_sin_0_1_N <- cpt_list$historia_mejores[1:i, -1:-2]
  historia_mejores_sin_0_1_N <- historia_mejores_sin_0_1_N[
    historia_mejores_sin_0_1_N > 0 & historia_mejores_sin_0_1_N < max(exceedances(cpt_list$data))
  ]
  plot(
    table(historia_mejores_sin_0_1_N) / cpt_list$param$r, 
    main = "Repeated change points", 
    ylab = "repetitions", 
    xlab = "change points index"
  )
}

#' @rdname grafica_datos_e_intervalos
#' @export
#' @examples
#' plot_best_chromosome(lista_AG)
plot_best_chromosome <- function(cpt_list) {
  plot(
    cpt_list$historia_mejores[, 1], 
    ylab = "Number of cp", 
    type = "l", 
    col = "blue", 
    main = paste(
      "The best chromosome with", 
      cpt_list$historia_mejores[which.min(cpt_list$vec_min_BMDL), 1], 
      "change points "
    )
  )
}

#' Graficando un ajuste de NHPP
#'
#' Esta función solo grafica un ajuste de NHPP
#'
#' @param d_i vector de días de revases
#' @param tau1 description
#' @param tau2 description
#' @param mat_phi description
#' @param mat_low_upp description
#' @export
#'
grafica_ajuste_NHPP <- function(d_i, tau1, tau2, initial_val_optim, mat_low_upp, rf_type, vec_dist_a_priori, mat_phi) {
  vec_d_i <- d_i[d_i > tau1 & d_i < tau2]
  
  val_optimos <- MAP_NHPP(initial_val_optim, mat_low_upp, vec_d_i, tau1, tau2, rf_type, vec_dist_a_priori, mat_phi)
  theta <- val_optimos$par
  
  t <- tau1:tau2
  tasa_NHPP <- mean_fn(t, rf_type, theta)
  upp_bond <- stats::qpois(.95, lambda = tasa_NHPP)
  low_bond <- stats::qpois(.05, lambda = tasa_NHPP)
  mean_NHPP <- tasa_NHPP
  
  # plot_confint
  grafica_escalonada(d_i, "black", vec_xlim = NULL, vec_ylim = range(0, length(d_i), upp_bond, low_bond, mean_NHPP))
  
  graphics::lines(t, upp_bond, col = "blue")
  graphics::lines(t, mean_NHPP, col = "red")
  graphics::lines(t, low_bond, col = "blue")
  
  cat("theta=", theta, "\n")
}

#' @rdname grafica_datos_e_intervalos
#' @export
#' @examples
#' plot_media_acumulada(lista_AG)
plot_media_acumulada <- function(cpt_list) {
  plot(cpt_list$data)
  
  tau <- cpt_best(cpt_list)
  graphics::abline(v = tau, lty = 3)
  
  m <- media_acumulada(cpt_list$data, tau = cpt_best(cpt_list), theta = cpt_best_params(cpt_list))
  graphics::lines(x = 1:nrow(m), y = m$m)
}