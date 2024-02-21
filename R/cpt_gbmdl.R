#' Class for candidate changepoints using Genetic BMDL heuristic
#' 
#' @param x an object coercible into a time series object via [stats::as.ts()]
#' @param ... arguments passed to methods
#' @export
#' @examples
#' cpts <- cpt_gbmdl(DataCPSim)
#' str(cpts)

cpt_gbmdl <- function(x, ...) {
  obj <- new_cpt_gbmdl(x, ...)
  validate_cpt_gbmdl(obj)
}

#' @rdname cpt_gbmdl
#' @export

validate_cpt_gbmdl <- function(x) {
  if (!stats::is.ts(as.ts(x))) {
    stop("data attribute is not coercible into a ts object.")
  }
  x
}

#' @rdname cpt_gbmdl
#' @param x a numeric vector
#' @param num_generations número de generaciones
#' @param generation_size tamaño de las generaciones
#' @param nhpp_dist toma valores en c("W","EW","GGO","MO","GO") y es el nombre de
#'   la función de tasa del NHPP
#' @param vec_dist_a_priori vector de los nobmres de las distribuciones a priori
#'   que se utilizan; eg c("Gamma","Gamma") y c("Gamma","Gamma","Gamma")
#' @param mat_phi matriz cuyos renglones tiene los parámetros de las
#'   distribuciones a priori; cada renglón tiene todos los parametros de una
#'   distribución
#' @export

new_cpt_gbmdl <- function(x = numeric(), 
                          nhpp_dist = c("W","EW","GGO","MO","GO")[1], # función de tasa de NHPP
                          vec_dist_a_priori = c("Gamma", "Gamma"), # distribuciones a priori
                          mat_phi = matrix(c(1, 3, 2, 1.2), ncol = 2),
                          num_generations = 50, 
                          generation_size = 50, 
                          max_num_cp = 20) {
  stopifnot(is.numeric(x))
  structure(
    list(
      data = stats::as.ts(x),
      nhpp_dist = nhpp_dist, 
      vec_dist_a_priori = vec_dist_a_priori,
      mat_phi = mat_phi,
      # 1. Simular puntos de cambio iniciales
      mat_cp = sim_k_cp_BMDL(x, generation_size),
      # historia_mejores guarda los mejores cp de cada generación
      historia_mejores = matrix(0, num_generations, max_num_cp),
      # vec_min_BMDL guarda los valores mínimos del MDL de cada generación
      vec_min_BMDL = rep(0, num_generations)
    ), 
    class = "cpt_gbmdl"
  )
}

#' @rdname cpt_gbmdl
#' @export

plot.cpt_gbmdl <- function(x, ...) {
  # 4-up plot
  plot_gbmdl(x)
}

#' @rdname cpt_gbmdl
#' @export

print.cpt_gbmdl <- function(x, ...) {
  NextMethod()
}

#' @rdname cpt_gbmdl
#' @export

summary.cpt_gbmdl <- function(object, ...) {
  message("List of changepoints object:")
  cat(paste("\nn:"), length(object))
  cat(paste("\nBest changepoint set: "))
  cat(cpt_best(object))
  cat(paste("\nNumber of changepoints:"), length(cpt_best(object)))
}

#' @rdname cpt_gbmdl
#' @export
nobs.cpt_gbmdl <- function(object, ...) {
  length(as.ts(object))
}

#' @rdname cpt_gbmdl
#' @export
logLik.cpt_gbmdl <- function(object, ...) {
  regions <- fit_nhpp(object, tau = changepoints(object))
  log_likes <- sum(regions$logLik)
  attr(log_likes, "df") <- length(changepoints(object))
  class(log_likes) <- "logLik"
  return(log_likes)
}

#' @rdname cpt_gbmdl
#' @export

cpt_best_bmdl <- function(x) {
  # Obtenemos el valor minimo de todas las evaluaciones
  min(x$vec_min_BMDL)
}

#' @rdname cpt_gbmdl
#' @export
cpt_best_bmdl_string <- function(x) {
  paste0("_BMDL_", floor(min(x$vec_min_BMDL)))
}

#' @rdname cpt_gbmdl
#' @export
#' @examples
#' cpt_best_params(lista_AG$segmenter)

cpt_best_params <- function(x) {
  fit_nhpp(as.ts(x), tau = cpt_best(x))
}

#' @rdname cpt_gbmdl
#' @export

chromosome_best <- function(x) {
  where_minimo_BMDL <- which.min(x$vec_min_BMDL)
  chromo_long <- x$historia_mejores[where_minimo_BMDL, ]
  chromo_long[1:(chromo_long[1] + 3)]
}

#' @rdname cpt_gbmdl
#' @export
#' 

cpt_best <- function(x) {
  chromo <- chromosome_best(x)
  k <- chromo[1]
  # trim the endpoints
  setdiff(chromo[3:(k + 2)], c(0, length(x)))
}

#' @rdname cpt_gbmdl
#' @export
#' @examples
#' chromo <- c(4, 1, 557, 877 , 905, 986, 1096, 0, 0, 0)
#' chromo2tau(chromo)

chromo2tau <- function(chromo) {
  k <- chromo[1]
  # trim the endpoints
  setdiff(chromo[3:(k + 2)], c(0, max(chromo)))
}

#' @rdname cpt_gbmdl
#' @export
mat_cp_2_tbl <- function(mat_cp) {
  mat_cp |>
    mat_cp_2_list() |>
    tibble::tibble() |>
    stats::setNames("tau") |>
    dplyr::mutate(
      m = purrr::map_int(tau, length)
    )
}

#' @rdname cpt_gbmdl
#' @export
mat_cp_2_list <- function(mat_cp) {
  mat_cp |>
    apply(1, chromo2tau)
}

#' @rdname cpt_gbmdl
#' @export
max_num_cp <- function(x) {
  ncol(x$mat_cp)
}

#' @rdname cpt_gbmdl
#' @export
generation_size <- function(x) {
  nrow(x$mat_cp)
}

#' @rdname cpt_gbmdl
#' @export
num_generations <- function(x) {
  length(x$vec_min_BMDL)
}

#' @rdname cpt_gbmdl
#' @export
#' @examples
#' write_cpt_gbmdl(lista_AG$segmenter)

write_cpt_gbmdl <- function(x, destdir = tempdir()) {
  
  dir_data <- fs::path(destdir)
  if (!dir.exists(dir_data)) {
    dir.create(dir_data, recursive = TRUE)
  }
  
  file_name <- paste0("Dat_AGBMDL_", file_name(x))
  
  # Write data
  save(x, file = fs::path(dir_data, file_name))
  message("Se guardo el archivo:\n", fs::path(dir_data, file_name), "\n")
}

#' @rdname cpt_gbmdl
#' @param data_name_slug character string that will identify the data set used
#' in the file name
#' @export
#' @examples
#' file_name(cpt_gbmdl(DataCPSim))

file_name <- function(x, data_name_slug = "data") {
  paste(
    "gbmdl", data_name_slug, 
    "nhpp", x$nhpp_dist, 
    gsub(" ", "_", label_priors(x)), 
    "r", num_generations(x), 
    "k", generation_size(x),
    cpt_best_bmdl_string(x), 
    ".rda", 
    sep = "_"
  )
}

#' Broom compatibility layer for changepoint
#' @param x A `cpt_gbmdl` object
#' @param ... arguments passed to methods
#' @export
#' @examples
#' cpts <- lista_AG
#' y <- augment(lista_AG)
#' class(y)
#' y
#' tidy(cpts)
#' glance(cpts)

glance.cpt_gbmdl <- function(x, ...) {
  tibble::tibble(
    pkg = "tidychangepoint",
    version = utils::packageVersion("tidychangepoint"),
    algorithm = "GeneticBMDL",
    test_stat = cpt_best_bmdl(x),
    BMDL = cpt_best_bmdl(x),
    logLik = logLik(x),
    AIC = AIC(x),
    BIC = BIC(x),
    num_cpts = length(cpt_best(x)),
  )
}

#' @rdname glance.cpt_gbmdl
#' @export
#' @examples
#' as.ts(lista_AG)
as.ts.cpt_gbmdl <- function(x, ...) {
  as.ts(x$data)
}

#' @rdname glance.cpt_gbmdl
#' @export
#' @examples
#' length(lista_AG)
#' 
length.cpt_gbmdl <- function(x, ...) {
  length(as.ts(x))
}

#' @rdname glance.cpt_gbmdl
#' @export
changepoints.cpt_gbmdl <- function(x, ...) {
  cpt_best(x) |>
    as.integer()
}

#' @rdname glance.cpt_gbmdl
#' @export
#' @examples
#' diagnose(lista_AG)
#' 
diagnose.cpt_gbmdl <- function(x, ...) {
  plot_gbmdl(x)
}

