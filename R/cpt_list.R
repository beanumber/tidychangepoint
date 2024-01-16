#' Class for candidate changepoints
#' @export
#' @param x a numeric vector
#' @examples
#' cpts <- cpt_list(DataCPSim, param)
#' str(cpts)
#' plot(cpts)

new_cpt_list <- function(x = numeric(), param = list()) {
  stopifnot(is.numeric(x))
  structure(
    list(
      data = stats::as.ts(x),
      n = length(x),
      param = param,
      # Inicializamos lista_AG_BMDL (de esta manero lo podemos meter en el for)
      # 1. Simular puntos de cambio iniciales
      lista_AG_BMDL = list(mat_cp = sim_k_cp_BMDL(exceedances(x), param)),
      # historia_mejores guarda los mejores cp de cada generación
      historia_mejores = matrix(0, param$r, param$max_num_cp),
      # vec_min_BMDL guarda los valores mínimos del MDL de cada generación
      vec_min_BMDL = rep(0, param$r)
    ), 
    class = "cpt_list"
  )
}

#' @rdname new_cpt_list
#' @export

validate_cpt_list <- function(x) {
  if (!stats::is.ts(x$data)) {
    stop("data attribute is not coercible into a ts object.")
  }
  x
}

#' @rdname new_cpt_list
#' @export

cpt_list <- function(x, ...) {
  obj <- new_cpt_list(x, ...)
  validate_cpt_list(obj)
}

#' @rdname new_cpt_list
#' @export

plot.cpt_list <- function(x, ...) {
  plot(x$data)
#  plot_cpt_repetidos(x)
  # 4-up plot
  # plot_BMDL(x)
}

#' @rdname new_cpt_list
#' @export
#' @examples
#' write_cpt_list(lista_AG)

write_cpt_list <- function(x, destdir = tempdir()) {
  
  dir_data <- fs::path(destdir, x$param$nombre_carpeta_RData)
  if (!dir.exists(dir_data)) {
    dir.create(dir_data, recursive = TRUE)
  }
  
  file_name <- paste0(
    "Dat_AGBMDL_", x$param$nombre_datos, "_rf_",
    x$param$rf_type, "_", fun_n_genera_texto_dist(x$param), "_r",
    x$param$r, "_k",
    x$param$k, x$valor_BMDL_minimo, ".RData"
  )
  
  # Write data
  save(x, file = fs::path(dir_data, file_name))
  message("Se guardo el archivo:\n", fs::path(dir_data, file_name), "\n")
}
