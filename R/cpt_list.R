#' Class for candidate changepoints
#' @export
#' @param x a numeric vector
#' @examples
#' cpts <- cpt_list(rlnorm_ts_1)
#' str(cpts)
#' plot(cpts)

new_cpt_list <- function(x = numeric()) {
  stopifnot(is.numeric(x))
  structure(
    list(
      data = as.ts(x),
      n = length(data)
    ), 
    class = "cpt_list"
  )
}

#' @rdname new_cpt_list
#' @export

validate_cpt_list <- function(x) {
  if (!is.ts(x$data)) {
    stop("data attribute is not coercible into a ts object.")
  }
  x
}

#' @rdname new_cpt_list
#' @export

cpt_list <- function(x) {
  obj <- new_cpt_list(x)
  validate_cpt_list(obj)
}

#' @rdname new_cpt_list
#' @export

plot.cpt_list <- function(x, ...) {
  plot(x$data)
}

#' @rdname new_cpt_list
#' @export
#' @examples
#' lista_AG <- AG_BMDL_r_paso(DataCPSimRebases, param)
#' write_cpt_list(lista_AG)

write_cpt_list <- function(x, destdir = tempdir()) {
  
  dir_data <- fs::path(destdir, x$param$nombre_carpeta_RData)
  if (!dir.exists(dir_data)) {
    dir.create(dir_data, recursive = TRUE)
  }
  
  nombre_archivo_AG <- paste0(
    "Dat_AGBMDL_", x$param$nombre_datos, "_rf_",
    x$param$rf_type, "_", fun_n_genera_texto_dist(x$param), "_r",
    x$param$r, "_k",
    x$param$k, lista_AG$valor_BMDL_minimo, ".RData"
  )
  
  # Write data
  save(lista_AG, file = fs::path(dir_data, nombre_archivo_AG))
  message("Se guardo el archivo:\n", fs::path(dir_data, nombre_archivo_AG), "\n")
}
