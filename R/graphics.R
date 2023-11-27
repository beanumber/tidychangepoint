#' Grafica los n puntos de cambio más repetidos sobre el plot.stepfun de los datos
#'
#' @param lista_AG description
#' @return guarda un pdf con la gráfica
#' @export
#' @examples
#' \dontrun{
#' lista_AG <- AG_BMDL_r_paso(DataCPSimRebases, param)
#' graf_puntos_cambio_repetidos(lista_AG)
#' }
#' 
graf_puntos_cambio_repetidos <- function(lista_AG, destdir = tempdir()) {
  # Obtenemos cuantos de los cp mejores se graficarán
  n_cp_mas_repetidos <- lista_AG$param$cuantos_mejores_cp_graf
  historia_mejores_sin_0_1_N <- lista_AG$historia_mejores[, -1:-2]
  historia_mejores_sin_0_1_N <- historia_mejores_sin_0_1_N[historia_mejores_sin_0_1_N > 0 & historia_mejores_sin_0_1_N < max(lista_AG$x)]
  cp_mas_repetidos <- rev(sort(table(historia_mejores_sin_0_1_N)))[1:n_cp_mas_repetidos]
  stats::plot.stepfun(lista_AG$x,
                      col.vert = "gray20", main = paste0("Los ", n_cp_mas_repetidos, " CP mas repetidos ", fun_n_genera_texto_dist(lista_AG$param)),
                      xlim = range(lista_AG$x)
  )
  graphics::abline(v = as.numeric(names(cp_mas_repetidos)), col = "blue")

  nombre_pdf <- paste0(
    "Fig_CP_repetidos_", lista_AG$param$nombre_datos, "_rf_",
    lista_AG$param$rf_type, "_", fun_n_genera_texto_dist(lista_AG$param), "_r",
    lista_AG$param$r, "_k",
    lista_AG$param$k, lista_AG$valor_BMDL_minimo, ".pdf"
  )
  
  path <- fs::path(destdir, nombre_pdf)
  
  grDevices::dev.print(pdf, path, width = 16, height = 10)
  message("Se guardo la imagen:\n", path, "\n")
}


#' Grafica escalonada para un proceso poisson
#'
#' @param res vector que contiene los lugares donde se quiere un escalón de tamaño 1
#' @param col_segmets color de las lineas
#' @param vec_xlim limites en el eje x de la gráfica
#' @param vec_ylim limites en el eje y de la gráfica
#' @export
#'
#' @examples
#' res <- c(10, 20, 33, 36, 43, 51, 56, 69, 82, 89)
#' grafica_escalonada(res, col_segmets = "blue")
#'
grafica_escalonada <- function(res, col_segmets, vec_xlim = NULL, vec_ylim = NULL) {
  lines_segment_NHPP <- function(n, res, col_segmets) {
    res <- c(0, res)
    for (i in 1:n) {
      graphics::segments(res[i], i - 1, res[i + 1], i - 1, col = col_segmets)
      graphics::segments(res[i + 1], i - 1, res[i + 1], i, col = col_segmets)
    }
  }
  
  
  n <- length(res)
  # Caso en que no hay vec_xlim
  if (is.null(vec_xlim)) vec_xlim <- range(0, res)
  # Caso en que no hay vec_ylim
  if (is.null(vec_ylim)) vec_ylim <- c(0, n)
  
  # Graficando
  plot(0,
       ylim = vec_ylim, xlim = vec_xlim, type = "n",
       xlab = "Time units - t", ylab = "Number of exceedances at t",
       main = expression(bold(paste("(a) Confidence Interval for fitted m(t|", theta, ")")))
  )
  
  lines_segment_NHPP(n, res, col_segmets)
}


#' Graficas de los intervalos y los datos
#'
#' @param lista_AG description
#' @param param description
#' @export
#' @examples
#' x <- sample(1:1096)
#' alpha <- runif(6) 
#' sigma <- runif(6)
#' tau <- sample(x, 5)
#' grafica_datos_e_intervalos(
#'   lista_AG = list(x = x), param,
#'   n_puntos_cambio = 5, d = x, 
#'   alpha = alpha, sigma = sigma, tau = tau
#' )
#' 
grafica_datos_e_intervalos <- function(lista_AG, param, n_puntos_cambio, d, alpha, sigma, tau) {
  # What is d????
  d <- 5
  
  # sink("funcion_media_acumulada.txt")
  gen_texto_m(n_puntos_cambio, mas_derecha = "")
  # sink()
  grafica_escalonada(lista_AG$x, "black")
  
  tasa_NHPP <- funcion_media_acumulada(i = 2, d, alpha, sigma, tau)
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
#' \dontrun{
#' lista_AG <- AG_BMDL_r_paso(DataCPSimRebases, param)
#' graficas_BMDL(lista_AG, param)
#' }
#' 
graficas_BMDL <- function(lista_AG, param, destdir = tempdir()) {
  graphics::par(mfrow = c(2, 2), mar = c(2, 4, 2, 2))
  # 1.- Datos reales
  # plot.stepfun(lista_AG$x, col.vert = "gray20",main="Data",xlim = range(lista_AG$x),ylab="m(t)")
  d <- lista_AG$x
  mejor_cp <- lista_AG$cromosoma_minimo_BMDL
  n_puntos_cambio <- mejor_cp[1]
  tau <- mejor_cp[3:(n_puntos_cambio + 2)]
  
  cromo <- mejor_cp[1:(n_puntos_cambio + 3)]
  # cp<-cromo[-c(1,2,(n_puntos_cambio+3))]
  mat_MAP <- extrae_mat_MAP(
    cromo, lista_AG$x, param$rf_type, param$initial_val_optim,
    param$mat_low_upp, param$vec_dist_a_priori, param$mat_phi
  )
  
  sigma <- mat_MAP[, 3]
  alpha <- mat_MAP[, 2]
  grafica_datos_e_intervalos(lista_AG, param, n_puntos_cambio, d, alpha, sigma, tau)
  
  # 2.- Evolución del algoritmo genético
  plot(lista_AG$vec_min_BMDL,
       xlim = c(1, param$r), type = "l", col = "blue", ylab = "BMDL", xlab = "Generation", main = paste0("AG rate ", param$rf_type, " and priori ", paste(param$vec_dist_a_priori, collapse = "-"))
  )
  # 3.- Puntos de cambio que más se repitieron
  historia_mejores_sin_0_1_N <- lista_AG$historia_mejores[, -1:-2]
  historia_mejores_sin_0_1_N <- historia_mejores_sin_0_1_N[historia_mejores_sin_0_1_N > 0 & historia_mejores_sin_0_1_N < max(lista_AG$x)]
  plot(table(historia_mejores_sin_0_1_N) / param$r, main = "Repeated change points", ylab = "repetitions", xlab = "change points index")
  # 4.- Number of change points in the best chromosomes
  plot(lista_AG$historia_mejores[, 1], ylab = "Number of cp", type = "l", col = "blue", main = paste0("The best chromosomes with", lista_AG$historia_mejores[which.min(lista_AG$vec_min_BMDL), 1], "change points "))
  graphics::par(mfrow = c(1, 1))
  
  nombre_pdf <- paste0(
    "Fig_4AGBMDL_", param$nombre_datos, "_rf_",
    param$rf_type, "_", fun_n_genera_texto_dist(param), "_r",
    param$r, "_k",
    param$k, lista_AG$valor_BMDL_minimo, ".pdf"
  )
  
  grDevices::dev.print(pdf, fs::path(destdir, nombre_pdf), width = 16, height = 10)
  message("Se guardo la imagen:\n", fs::path(destdir, nombre_pdf), "\n")
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
  
  grafica_escalonada(d_i, "black", vec_xlim = NULL, vec_ylim = range(0, length(d_i), upp_bond, low_bond, mean_NHPP))
  
  graphics::lines(t, upp_bond, col = "blue")
  graphics::lines(t, mean_NHPP, col = "red")
  graphics::lines(t, low_bond, col = "blue")
  
  cat("theta=", theta, "\n")
}
