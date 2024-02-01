#' Class for candidate changepoints using Genetic BMDL heuristic
#' @export
#' @param x a numeric vector
#' @examples
#' cpts <- cpt_gbmdl(DataCPSim, param)
#' str(cpts)
#' plot(cpts)

new_cpt_gbmdl <- function(x = numeric(), param = list()) {
  stopifnot(is.numeric(x))
  structure(
    list(
      data = stats::as.ts(x),
#      n = length(x),
      param = param,
      # Inicializamos lista_AG_BMDL (de esta manero lo podemos meter en el for)
      # 1. Simular puntos de cambio iniciales
      lista_AG_BMDL = list(mat_cp = sim_k_cp_BMDL(exceedances(x), param)),
      # historia_mejores guarda los mejores cp de cada generación
      historia_mejores = matrix(0, param$r, param$max_num_cp),
      # vec_min_BMDL guarda los valores mínimos del MDL de cada generación
      vec_min_BMDL = rep(0, param$r)
    ), 
    class = "cpt_gbmdl"
  )
}

#' @rdname new_cpt_gbmdl
#' @export

validate_cpt_gbmdl <- function(x) {
  if (!stats::is.ts(x$data)) {
    stop("data attribute is not coercible into a ts object.")
  }
  x
}

#' @rdname new_cpt_gbmdl
#' @export

cpt_gbmdl <- function(x, ...) {
  obj <- new_cpt_gbmdl(x, ...)
  validate_cpt_gbmdl(obj)
}

#' @rdname new_cpt_gbmdl
#' @export

plot.cpt_gbmdl <- function(x, ...) {
  plot(x$data)
#  plot_cpt_repetidos(x)
  # 4-up plot
  plot_BMDL(x)
}

#' @rdname new_cpt_gbmdl
#' @export

print.cpt_gbmdl <- function(x, ...) {
  NextMethod()
}

#' @rdname new_cpt_gbmdl
#' @export

summary.cpt_gbmdl <- function(object, ...) {
  message("List of changepoints object:")
  cat(paste("\nn:"), length(object$data))
  cat(paste("\nBest changepoint set: "))
  cat(cpt_best(object))
  cat(paste("\nNumber of changepoints:"), length(cpt_best(object)))
}


#' @rdname new_cpt_gbmdl
#' @export

cpt_best_bmdl <- function(x) {
  # Obtenemos el valor minimo de todas las evaluaciones
  min(x$vec_min_BMDL)
}

#' @rdname new_cpt_gbmdl
#' @export
cpt_best_bmdl_string <- function(x) {
  paste0("_BMDL_", floor(min(x$vec_min_BMDL)))
}

#' @rdname new_cpt_gbmdl
#' @export
#' @examples
#' cpt_best_params(lista_AG)
#' 

cpt_best_params <- function(x) {
  # extrae_mat_MAP(
  #   chromosome_best(x), 
  #   x$data, 
  #   x$param$rf_type, 
  #   x$param$initial_val_optim, 
  #   x$param$mat_low_upp, 
  #   x$param$vec_dist_a_priori, 
  #   x$param$mat_phi
  # ) |>
  #   as.data.frame()
  
  fit_MAP(x$data, tau = cpt_best(x), param = x$param)
}

#' @rdname new_cpt_gbmdl
#' @export

chromosome_best <- function(x) {
  where_minimo_BMDL <- which.min(x$vec_min_BMDL)
  chromo_long <- x$historia_mejores[where_minimo_BMDL, ]
  chromo_long[1:(chromo_long[1] + 3)]
}

#' @rdname new_cpt_gbmdl
#' @export

cpt_best <- function(x) {
  chromo <- chromosome_best(x)
  k <- chromo[1]
  # trim the endpoints
  setdiff(chromo[3:(k + 2)], c(0, length(as.ts(x))))
}

#' @rdname new_cpt_gbmdl
#' @export
#' @examples
#' write_cpt_gbmdl(lista_AG)

write_cpt_gbmdl <- function(x, destdir = tempdir()) {
  
  dir_data <- fs::path(destdir, x$param$nombre_carpeta_RData)
  if (!dir.exists(dir_data)) {
    dir.create(dir_data, recursive = TRUE)
  }
  
  file_name <- paste0(
    "Dat_AGBMDL_", x$param$nombre_datos, "_rf_",
    x$param$rf_type, "_", fun_n_genera_texto_dist(x$param), "_r",
    x$param$r, "_k",
    x$param$k, 
    cpt_best_bmdl_string(x), ".RData"
  )
  
  # Write data
  save(x, file = fs::path(dir_data, file_name))
  message("Se guardo el archivo:\n", fs::path(dir_data, file_name), "\n")
}

