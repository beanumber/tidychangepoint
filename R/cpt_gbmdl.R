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
  out <- new_seg_default(x, params = list(
    num_generations = num_generations, 
    nhpp_dist = nhpp_dist, 
    vec_dist_a_priori = vec_dist_a_priori,
    mat_phi = mat_phi
  ))
  out$mat_cp <- sim_k_cp_BMDL(x, generation_size)
  
  class(out) <- c("cpt_gbmdl", class(out))
  return(out)
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
    apply(1, chromo2tau, simplify = FALSE)
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
  x$params$num_generations
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
  NextMethod(x) |>
    dplyr::mutate(algorithm = "GeneticBMDL")
}


