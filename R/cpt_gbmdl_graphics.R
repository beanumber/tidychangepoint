#' Grafica los n puntos de cambio más repetidos sobre el plot.stepfun de los datos
#'
#' @param x A `cpt_gbmdl` object
#' @param destdir Directory in which to store the plots
#' @param data_name_slug character vector added to the file name
#' @param pdf Save the plot as a PDF? 
#' @param limit al final se generan unas graficas de los
#'   mejores puntos de cambio, este parámetro dicta cuantos cromosomas se
#'   graficarán
#' @return guarda un pdf con la gráfica
#' @export
#' @examples
#' plot_cpt_repetidos(lista_AG$segmenter)
#' 
plot_cpt_repetidos <- function(x, destdir = tempdir(), 
                               data_name_slug = "data", pdf = FALSE, 
                               limit = 100) {
  # Obtenemos cuantos de los cp mejores se graficarán
  historia_mejores_sin_0_1_N <- x$historia_mejores[, -1:-2]
  historia_mejores_sin_0_1_N <- historia_mejores_sin_0_1_N[historia_mejores_sin_0_1_N > 0 & historia_mejores_sin_0_1_N < max(as.ts(x))]
  cp_mas_repetidos <- rev(sort(table(historia_mejores_sin_0_1_N)))[1:limit]
  stats::plot.stepfun(
    as.ts(x),
    col.vert = "gray20", 
    main = paste0("Los ", limit, " CP mas repetidos ", label_priors(x)),
    xlim = range(as.ts(x))
  )
  graphics::abline(v = as.numeric(names(cp_mas_repetidos)), col = "blue")

  filename_pdf <- paste0("Fig_CP_repetidos_", file_name(x))
  
  path <- fs::path(destdir, filename_pdf)
  
  if (pdf) {
    grDevices::dev.print(pdf, path, width = 16, height = 10)
  }
  message("Se guardo la imagen:\n", path, "\n")
}


#' @rdname plot_cpt_repetidos
#' @return una gráfica con :
#' 1 Datos reales
#' 2 Evolución del algoritmo genético
#' 3 Puntos de cambio que más se repitieron
#' 4 Number of change points in the best chromosomes
#'
#' @export
#' @examples
#' plot_gbmdl(lista_AG$segmenter)
#' 
plot_gbmdl <- function(x, destdir = tempdir(), data_name_slug = "data", pdf = FALSE) {
  graphics::par(mfrow = c(2, 2), mar = c(2, 4, 2, 2))
  # 1.- Datos reales
  plot_confint(x)
  # 2.- Evolución del algoritmo genético
  plot_evolution(x)
  # 3.- Puntos de cambio que más se repitieron
  plot_cpt_repeated(x)
  # 4.- Number of change points in the best chromosomes
  plot_best_chromosome(x)

  graphics::par(mfrow = c(1, 1))
  
  filename_pdf <- paste0("Fig_4AGBMDL_", file_name(x))
  
  if (pdf) {
    grDevices::dev.print(pdf, fs::path(destdir, filename_pdf), width = 16, height = 10)
  }
  message("Se guardo la imagen:\n", fs::path(destdir, filename_pdf), "\n")
}

#' @rdname plot_cpt_repetidos
#' @export
#' @examples
#' plot_cpt_repeated(lista_AG$segmenter)
#' plot_cpt_repeated(lista_AG$segmenter, 5)
plot_cpt_repeated <- function(x, i = nrow(x$candidates)) {
  freq <- x$candidates[1:i, ] |>
    dplyr::pull(changepoints) |>
    unlist() |>
    table()
  plot(
    freq / num_generations(x), 
    main = "Repeated change points", 
    ylab = "repetitions", 
    xlab = "change points index"
  )
}

