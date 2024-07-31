globalVariables(
  c("x", "N", "alpha", "d", "n_puntos_cambio", "param", "pdf", "sigma", "tau")
)
#-------------------------------------------------------------------------#
#                                                                         #
#             Funciones de algoritmos geneticos Bayesian MDL              #
#                                                                         #
#-------------------------------------------------------------------------#
# V01  1 oct 2019
# V02 30 oct 2019, tomando en cuento correcciones de Eliane en MDL
#-------------------------------------------------------------------------#
# OBSERVACIONES:

# 1. Se esta tomando en cosiderasión el artículo
# Multiple Changepoint Detection via Genetic Algorithms
#
# 2. En este artícul se presentan tres MDL (log-normal, AR(1) y Poisson)
# 3. Se tienen m puntos de cambio, los cuales \tau_0=1 y \tau_{m+1}= N+1, pero
# en nuestro caso tenemos que los vectores cp tienen
# c(m,\tau_0=1,\tau_2,...,\tau_{m-1},\tau_m= N,0,0,0) por lo cual se nosotros:
# 1) empieza con el número de puntos de cambio; 2) la segunda entrada es un uno;
# 3) la tercera entrada es el primer punto de cambio; 4) las siguientes son
# otros puntos de cambio; 5) la siguiente entrada, después del último punto de
# cambio tiene el valor  N; y 6) los siguientes son númores cero hasta llenarlo
# para que sea de tamaño max_num_cp
#
#-------------------------------------------------------------------------#


# FUNCIONES UTILIZADAS ----------------------------------------------------


#' Algoritmo genético de Bayesian MDL a un paso
#' @description
#' 
#' `r lifecycle::badge("deprecated")`
#' This implementation is deprecated. Please see [segment_ga_coen()]
#' 
#' @param x an object coercible into a time series object via [stats::as.ts()]
#' @param generation_size tamaño de las generaciones
#' @param nhpp_dist toma valores en c("W","EW","GGO","MO","GO") y es el nombre de
#'   la función de tasa del NHPP
#' @param vec_dist_a_priori vector de los nobmres de las distribuciones a priori
#'   que se utilizan; eg c("Gamma","Gamma") y c("Gamma","Gamma","Gamma")
#' @param mat_phi matriz cuyos renglones tiene los parámetros de las
#'   distribuciones a priori; cada renglón tiene todos los parametros de una
#'   distribución
#' @param max_num_cp el máximo número de rebases. Este parámetro se ocupa en
#'   particular para que todos los cromosomas quepan en una matriz.
#' @param ... arguments passed to methods
#' @param num_generations Number of generations to evolve
#' @param show_progress_bar show the progress bar?
#' @return A `cpt_gbmdl` object
#' @keywords internal
#' @export
#' @examples
#' \donttest{
#' x <- segment_coen(DataCPSim, num_generations = 5)
#' }
#' 
segment_coen <- function(x, 
                           num_generations = 50, 
                           nhpp_dist = c("W","EW","GGO","MO","GO")[1], # función de tasa de NHPP
                           vec_dist_a_priori = c("Gamma", "Gamma"), # distribuciones a priori
                           mat_phi = matrix(c(1, 3, 2, 1.2), ncol = 2),
                           generation_size = 50, 
                           max_num_cp = 20,
                           show_progress_bar = TRUE, ...) {
  lifecycle::deprecate_warn("0.0.1", "segment_coen()", "segment_ga_coen()")
  obj <- seg_basket(
    x, 
    algorithm = "Coen", 
    seg_params = list(
      num_generations = num_generations, 
      nhpp_dist = nhpp_dist, 
      vec_dist_a_priori = vec_dist_a_priori,
      mat_phi = mat_phi,
      generation_size = generation_size, 
      max_num_cp = max_num_cp
    ),
    model_name = "nhpp",
    penalty = "BMDL",
  )
  obj$mat_cp <- sim_k_cp_BMDL(x, generation_size)
  class(obj) <- c("cpt_gbmdl", class(obj))
  
  pb <- utils::txtProgressBar(min = 1, max = obj$seg_params$num_generations, style = 3, width = 60)
#  graphics::par(mfrow = c(2, 1), mar = c(1, 4, 2, 2))
  
  best_cpts <- list()
  
  for (i in 1:obj$seg_params$num_generations) {

    this_generation <- obj$mat_cp |>
      mat_cp_2_list() |>
      evaluate_cpts(.data = as.ts(obj), model_fn = fit_nhpp)
    
    best_cpts[[i]] <- this_generation |>
      dplyr::arrange(obj$penalty) |>
      utils::head(1)
    
    # Imprimimos el porcentaje de progreso
    if (show_progress_bar) {
      utils::setTxtProgressBar(pb, i)
    }
    
    obj$mat_cp <- obj |>
      exceedances() |>
      evolve_gbmdl(
        obj$mat_cp, 
        this_generation |> dplyr::pull(obj$penalty)
      )
    #    plot_evolution(obj, i)
    #    plot_cpt_repeated(obj, i)
  }
  close(pb)
#  graphics::par(mfrow = c(1, 1))
  obj$basket <- best_cpts |>
    dplyr::bind_rows()

  return(obj)
}

#' Generate a list of candidate changepoints using a genetic algorithm
#' 
#' @param x A time series object
#' @param mat_cp A matrix containing a list of candidate chromosomes
#' @param these_bmdls vector of [BMDL()] scores
#' @export
#' @keywords internal
#' @examples
#' mat_cp <- sim_k_cp_BMDL(DataCPSim)
#' bmdls <- mat_cp |> 
#'   mat_cp_2_list() |> 
#'   evaluate_cpts(.data = as.ts(DataCPSim), model_fn = fit_nhpp) |> 
#'   dplyr::pull(BMDL)
#' evolve_gbmdl(exceedances(DataCPSim), mat_cp, bmdls)

evolve_gbmdl <- function(x, mat_cp, these_bmdls) {
  # 2. Encontrar sus probabilidades
  vec_probs <- probs_vec_MDL(these_bmdls)
  # 3. Seleccionar dos padres
  mat_padres <- selec_k_pares_de_padres(vec_probs)
  # 4. Juntar sus puntos de cambio
  mat_cp <- junta_k_puntos_cambio(mat_padres, mat_cp)
  # 5. Volados para quitar puntos de cambio
  mat_cp <- mata_k_tau_volado(mat_cp)
  # 6. Mutaciones puntos de cambio
  mat_cp <- muta_k_cp_BMDL(mat_cp, x)
  # (mat_cp <- muta_k_cp(mat_cp,param)) # antes
  
  # 8. Regresa el resultado
  return(mat_cp)
}


#' @rdname evolve_gbmdl
#' @param padres vector de longitud dos con índice de papa e índice de mama
#' @export
junta_1_puntos_cambio <- function(padres, mat_cp) {
  # 1.- Juntamos todos los puntos de cambio de los padres; se quitan los puntos
  # de cambio repetidos y el [-1] final es para quitar los ceros
  (hijo <- sort(unique(c(mat_cp[padres, -1])))[-1])

  # 2.- Si son demasiados puntos de cambio los recortamos
  if (length(hijo) <= ncol(mat_cp) - 1) {
    # Caso donde no son demasiados puntos de cambio
    (hijo <- c(length(hijo) - 2, hijo, rep(0, ncol(mat_cp) - length(hijo) - 1)))
  } else {
    # Caso donde son demasiados puntos de cambio; se quitan de manera aleatoria
    # uniforme los puntos de cambio que excedan la longitud máxima
    hijo <- c(
      ncol(mat_cp) - 3, 1, sort(sample(hijo[c(-1, -length(hijo))],
        size = ncol(mat_cp) - 3, replace = F
      )),
      hijo[length(hijo)]
    )
  }
  return(hijo)
}


#' @rdname evolve_gbmdl
#' @param mat_padres matriz de kx2 la cual contiene en sus renglones las
#'   parejas de padres
#' @return regresa una matriz de las mismas dimensiones que mat_cp, pero con los
#'   nuevos cromosomas
#' @export
junta_k_puntos_cambio <- function(mat_padres, mat_cp) {
  k <- nrow(mat_cp) # k es el número de padres
  N <- ncol(mat_cp) # N es la dimensión del vector de observaciones
  mat_hijos <- matrix(0, k, N)
  for (i in 1:k) {
    mat_hijos[i, ] <- junta_1_puntos_cambio(mat_padres[i, ], mat_cp)
  }
  return(mat_hijos)
}


#' @rdname evolve_gbmdl
#' @param cp vector cromosoma que se va a poner a prueba
#' @param prob_volado probabilidad de quitar un tiempo de cambio existente
#'   utilizado por mata_k_tau_volado para quitar elementos de más. Se recomienda
#'   dejar el valor de 0.5 ya que así al juntar los pc del padre y madre se
#'   eliminará la mitad de estos
#'
#' @return el mismo cromosoma sin algunos de sus puntos de cambio
#' @export
mata_1_tau_volado <- function(cp, prob_volado = 0.5) {
  # (cp <- sim_1_cp(N,param) )
  # (m <- cp[1])
  # N <- cp[cp[1]+3]
  (cp_corto <- cp[2:(cp[1] + 3)])
  (cp_corto <- cp_corto[c(TRUE, as.logical(stats::rbinom(cp[1], 1, prob_volado)), TRUE)])
  cp <- c(length(cp_corto) - 2, cp_corto, rep(0, length(cp) - length(cp_corto) - 1))
  return(cp)
}

#' @rdname evolve_gbmdl
#' @return regresa una matriz a la cual se le quitaron a sus cromosomas algunos
#'   puntos de cambio
#' @export
mata_k_tau_volado <- function(mat_cp) {
  for (i in 1:nrow(mat_cp)) {
    mat_cp[i, ] <- mata_1_tau_volado(mat_cp[i, ])
  }
  return(mat_cp)
}

#' @rdname evolve_gbmdl
#' @param probs_nuevos_muta0N probabilidades de mutar 0,1,2,...,l hasta cierto
#'   numero l; eg si vale c(.5,.2,.2,.1) se tiene una probabilidad 0.5 de mutar
#'   0 (de no mutar), probabilidad 0.2 de mutar 1,, probabilidad 0.2 de mutar 2,
#'   y, probabilidad 0.1 de mutar 3.
#' @param dist_extremos distancia entre el primer los puntos de cambio v_0 y v_1
#'   al igual que entre v_m y v_\{m+1\}; distancia minima que debe de haber de un
#'   punto de cambio y los valores 1 y T, donde T es la longitud total de la
#'   serie
#' @param min_num_cpts es la cota inferior del número de puntos de cambio
#'   que puede tener un cromosoma
#' @param mutation_possibilities vector con mutaciones posibles; eg si mutaciones=c(-1,0,1)
#'   entonces un punto de cambio puede ser movido una unidad a la izquierda,
#'   puede quedarse igual, o moverse una unidad a la derecha
#' @param mutation_probs probabilidades de mutación. Las longitudes de este vector y
#'   mutaciones tienen que ser iguales; eg si mutaciones=c(-1,0,1) y probs_muta
#'   = c(.2, .6, .2) entonces se tiene una probabilidad .2 de que el punto de
#'   cambio se desplace a la izquierda, probabilidad .6 de quedar igual, y
#'   probabilidad . 2 de ser movido a la derecha
#' @param max_num_cp el máximo número de rebases. Este parámetro se ocupa en
#'   particular para que todos los cromosomas quepan en una matriz.
#' @return regresa un vector mutado
#' @export
#'
muta_1_cp_BMDL <- function(cp, x, 
                           probs_nuevos_muta0N = c(0.8, 0.1, 0.1), 
                           dist_extremos = 10, 
                           min_num_cpts = 1, 
                           mutation_possibilities = c(-1, 0, 1),
                           mutation_probs = c(0.3, 0.4, 0.3),
                           max_num_cp = 20
) {
  # (cp <- sim_1_cp_BMDL(x,param) )

  # En caso de tener muy pocos puntos de cambio, rehacemos el cp
  if (cp[1] <= min_num_cpts) {
    return(sim_1_cp_BMDL(x))
  }

  (cp_posibles_muta <- cp[3:(cp[1] + 2)])
  # Indices mutados
  i_mutados <- sort(unique(sapply(
    match(cp_posibles_muta, x) + sample(mutation_possibilities, size = cp[1], prob = mutation_probs, replace = T),
    function(yy) {
      min(max(yy, dist_extremos), length(x) - dist_extremos)
    }
  )))
  # Perturbamos los tau
  (cp_posibles_muta <- x[i_mutados])

  # Agregamos algunos puntos de cambio aleatoriamente
  if (length(cp_posibles_muta) < max_num_cp - 3) {
    # Simulamos cuantos vamos a agregar
    (cuantos_nuevos <- sample(0:(length(probs_nuevos_muta0N) - 1), size = 1, prob = probs_nuevos_muta0N))
    # print(cuantos_nuevos)
    if (length(cp_posibles_muta) < max_num_cp + cuantos_nuevos && cuantos_nuevos > 0) {
      cp_posibles_muta <- sort(unique(c(
        cp_posibles_muta,
        sample(x[dist_extremos:(length(x) - dist_extremos)], cuantos_nuevos)
      )))
    }
  }
  # cp_posibles_muta
  # length(cp_posibles_muta)

  # Agregamos las mutaciones a la estructura que utilizamos en los cromosomas
  cp <- c(length(cp_posibles_muta), 1, cp_posibles_muta, cp[cp[1] + 3], rep(0, max_num_cp - length(cp_posibles_muta) - 3))
  return(cp)
}


#' @rdname evolve_gbmdl
#' @return regreas una mat_cp mutada
#' @export
muta_k_cp_BMDL <- function(mat_cp, x) {
  mat_muta <- matrix(0, nrow(mat_cp), ncol(mat_cp))
  for (i in 1:nrow(mat_cp)) {
    mat_muta[i, ] <- muta_1_cp_BMDL(mat_cp[i, ], x)
  }
  return(mat_muta)
}




#' @rdname evolve_gbmdl
#' @details
#' regresa un vector de tamaño `max_num_cp+3` donde la primera entrada es
#'         m, la segunda \eqn{v_0=1, ...., v_{m+1}=N,0,...,0}
#'
#' por ejemplo: `c(4,1,3,8,11,15,20,0,0,0,0)` para `m=4`, \eqn{max\_num\_cp=8}, \eqn{N=20}.
#'         Se tienen `m` puntos de cambio, los cuales 
#'          \eqn{\tau_0=1} y \eqn{\tau_{m+1}= N+1}, 
#'         pero en nuestro caso
#'         tenemos que los vectores `cp` tienen \eqn{c(m,\tau_0=1,\tau_1,...,\tau_{m-1},\tau_m= N,0,0,0)}
#'         por lo cual se nosotros:
#' - empieza con el número de puntos de cambio;
#' - la segunda entrada es un uno;
#' - la tercera entrada es el primer punto de cambio;
#' - las siguientes son otros puntos de cambio;
#' - la siguiente entrada después de punto de cambio tiene el valor  `N`; y
#' - los siguientes son númores cero hasta llenarlo para que sea de tamaño `max_num_cp`
#' @param prob_inicial probabilidad de que en la primera generación un punto
#'   cualquiera sea punto de cambio. Se recomienda =.5 ya que con esto se
#'   distribuyen relativamente uniformes los puntos de cambio
#' @export
#' @examples
#' sim_1_cp_BMDL(exceedances(DataCPSim))
#' sim_1_cp_BMDL(exceedances(rlnorm_ts_1))
#' sim_1_cp_BMDL(exceedances(rlnorm_ts_2))
#' sim_1_cp_BMDL(exceedances(rlnorm_ts_3))
#' sim_1_cp_BMDL(exceedances(bogota_pm))
#'
sim_1_cp_BMDL <- function(x, max_num_cp = 20, prob_inicial = 0.06) {
  # Primero simulamos una binomial que va a ser el número de puntos de cambio
  m <- min(stats::rbinom(1, length(x), prob_inicial), max_num_cp - 3)
  # Simulamos los puntos de cambio uniformemente aleatorios
  good <- x[-length(x)]
  if (1 %in% good) {
    good <- good[2:length(good)]
  }
  valores_cp <- sort(sample(good, size = m, replace = F))
  # Genera cromosoma con estructura manejable
  ans <- c(m, 1, valores_cp, max(x), rep(0, max_num_cp - m - 3))
  return(ans)
}


#' @rdname evolve_gbmdl
#' @param generation_size tamaño de las generaciones
#' @return regresa una matriz de `k` por `max_num_cp+3`, la cual en cada renglón tiene
#'         una simulación de un vector de tiempos de cambio
#' @export
#' @examples
#' sim_k_cp_BMDL(DataCPSim)
#'
sim_k_cp_BMDL <- function(x, generation_size = 50, max_num_cp = 20) {
  mat_cp <- matrix(0, generation_size, max_num_cp)
  exc <- exceedances(x)
  for (i in 1:generation_size) {
    mat_cp[i, ] <- sim_1_cp_BMDL(exc)
  }
  return(mat_cp)
}



#' @rdname evolve_gbmdl
#' @param vec_MDL vector con valores MDL
#'
#' OBSERVACIÓN: Esto regresa numeros negativos, los cuales mientras más negativo mejor, ya que
#'             dará que es un mejor vector de tiempos de cambio. Es decir, un MDL de -6000 es
#'             mejor que -4000
#' @param probs_rank0_MDL1 para medir obtener la probabilidad de los padres se
#'   pueden tomar o las probabilidades con respecto a los rangos (como en el
#'   artículo) o se pueden tomar las probabilidades con respecto a el MDL. La
#'   diferencia radica en que si se toma con respecto al MDL se tendrá que un
#'   cromosoma con un gran MDL este tendrá una gran ventaja de ocurrir, en
#'   cambio cuando solo se tiene rank esta ventaja gran ventaja se reduce
#'
#' @return regresa un vector de probabilidades
#' @export
probs_vec_MDL <- function(vec_MDL, probs_rank0_MDL1 = 0) {
  if (any(is.infinite(vec_MDL))) {
    warning("Valor infinito; fun probs_vec_MDL, vec_MDL=")
    message(vec_MDL)
  }
  if (probs_rank0_MDL1 == 0) {
    return(rank(-vec_MDL))
  }
  if (probs_rank0_MDL1 == 1) {
    return(-vec_MDL)
  }
}


#' @rdname evolve_gbmdl
#' @param vec_probs vector de probabilidades de selección de cada uno de los
#'   cromosomas
#' @export
#'
selec_k_pares_de_padres <- function(vec_probs) {
  k <- length(vec_probs)
  (papas <- sample(1:k, size = k, prob = vec_probs, replace = T))
  (mamas <- sample(1:k, size = k, prob = vec_probs, replace = T))
  # Eliminamos los casos en los que los padres son el mismo cromosoma
  (indices_mal <- which(papas == mamas))
  if (length(indices_mal) > 0) {
    for (i in indices_mal) {
      mamas[i] <- sample((1:k)[-papas[i]],
        size = 1,
        prob = vec_probs[-papas[i]], replace = T
      )
    }
  }
  return(matrix(c(papas, mamas), ncol = 2))
}

#' @rdname evolve_gbmdl
#' @param chromo Chromosome, from a row of the matrix `mat_cp`
#' @export
#' @examples
#' chromo <- c(4, 1, 557, 877 , 905, 986, 1096, 0, 0, 0)
#' chromo2tau(chromo)

chromo2tau <- function(chromo) {
  k <- chromo[1]
  # trim the endpoints
  setdiff(chromo[3:(k + 2)], c(0, max(chromo)))
}

#' @rdname evolve_gbmdl
#' @param mat_cp A matrix of potential changepoints
#' @export
mat_cp_2_list <- function(mat_cp) {
  mat_cp |>
    apply(1, chromo2tau, simplify = FALSE)
}
