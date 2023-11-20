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



# cat("\014") # borra consola
# rm(list=ls())  # Borra todo

# library(openxlsx)
# library(rio)


#' Validador de la lista param para ejecutar el AG-BMDL
#'
#' @param param es la lista original de parámetros la cual contiene todos los
#'   siguientes
#' @param nombre_datos esta variable indica la base de datos que se va a ocupar.
#'   Esta base se toma del archivo "Datos.R"; los posibles valores que toma son
#'   c("ciclones","O3","O3_U61","PM10","PM10_U100")
#' @param frecuencia_datos indica se hay que hacer un cambio en la frecuencia de
#'   los datos. En particular, se utiliza junto con la variable
#'   diarios0_rebases1 = 1, (si esta última vale 0, frecuencia_datos no tiene
#'   efecto)
#' @param n_datos Es una variable para hacer pruebas que cirve para tocar un
#'   subconjunto de los datos, o en caso de ser "TODOS" tomar todos los datos
#' @param diarios0_rebases1 indicadora si se toman los revasos o los datos
#'   crudos. En particular se utiliza para el caso de dist="poisson", ya que al
#'   tomar los datos distribuidos poisson se estará contando el número de
#'   revases
#' @param valor_de_rebase es la indicadora del nivel para revase, por lo general
#'   vale 100
#' @param r número de generaciones
#' @param k tamaño de las generaciones
#' @param penalty tipo de penalidad que se ocupa, por ahora solo se tiene
#'   programado MDL
#' @param max_num_cp el máximo número de rebases. Este parámetro se ocupa en
#'   particular para que todos los cromosomas quepan en una matriz.
#' @param prob_inicial probabilidad de que en la primera generación un punto
#'   cualquiera sea punto de cambio. Se recomienda =.5 ya que con esto se
#'   distribuyen relativamente uniformes los puntos de cambio
#' @param prob_volado probabilidad de quitar un tiempo de cambio existente
#'   utilizado por mata_k_tau_volado para quitar elementos de más. Se recomienda
#'   dejar el valor de 0.5 ya que así al juntar los pc del padre y madre se
#'   eliminará la mitad de estos
#' @param probs_muta probabilidades de mutación. Las longitudes de este vector y
#'   mutaciones tienen que ser iguales; eg si mutaciones=c(-1,0,1) y probs_muta
#'   = c(.2, .6, .2) entonces se tiene una probabilidad .2 de que el punto de
#'   cambio se desplace a la izquierda, probabilidad .6 de quedar igual, y
#'   probabilidad . 2 de ser movido a la derecha
#' @param mutaciones vector con mutaciones posibles; eg si mutaciones=c(-1,0,1)
#'   entonces un punto de cambio puede ser movido una unidad a la izquierda,
#'   puede quedarse igual, o moverse una unidad a la derecha
#' @param dist_extremos distancia entre el primer los puntos de cambio v_0 y v_1
#'   al igual que entre v_m y v_{m+1}; distancia minima que debe de haber de un
#'   punto de cambio y los valores 1 y T, donde T es la longitud total de la
#'   serie
#' @param prob_para_sin_cp En caso de querer mutar un cp sin puntos de cambio se
#'   lanza un volado con probabilidad prob_para_sin_cp, si es cae 1 se regresa
#'   el cp original, si cae 0 se simula un punto de cambio y se regresa este cp
#'   con este punto de cambio
#' @param cp_real en caso de estar haciendo pruebas y conocer los puntos de
#'   cambio reales da un vector de tamaño max_num_cp con entradas
#'   m,tau_0,...,tau_{m+1},0,...,0, en particual esto cirve para que al final se
#'   grafiquen los puntos de cambio reales contra los estimados; en caso de no
#'   conocer los pc reales, esta variable tiene el valor "sin cp_real"
#' @param quita_ini0_fin1 es una indicadora para cuando se hacen rebases y la
#'   serie de tiempo x (la serie original) no es exanctamente de la longitud de
#'   frecuencia_datos. Entonces se quitan los restantes del final o del
#'   principio
#' @param probs_rank0_MDL1 para medir obtener la probabilidad de los padres se
#'   pueden tomar o las probabilidades con respecto a los rangos (como en el
#'   artículo) o se pueden tomar las probabilidades con respecto a el MDL. La
#'   diferencia radica en que si se toma con respecto al MDL se tendrá que un
#'   cromosoma con un gran MDL este tendrá una gran ventaja de ocurrir, en
#'   cambio cuando solo se tiene rank esta ventaja gran ventaja se reduce
#' @param my_data en caso de que se quiera hacer pruebas con otra serie en esta
#'   variable se carga la serie con la que se desea hacer las pruebas. En este
#'   caso, se comenta la entrada llamada my_data para que se pueda correr la
#'   función
#' @param nombre_carpeta_pdf normalmente esta carpeta se llama "Figures", y
#'   contiene las figuras que genera el programa
#' @param nombre_carpeta_RData normalmente esta carpeta se llama "Data", y
#'   contiene archivos .RData que tienen los resultados del programa
#' @param cuantos_mejores_cp_graf al final se generan unas graficas de los
#'   mejores puntos de cambio, este parámetro dicta cuantos cromosomas se
#'   graficarán
#' @param minimo_numero_de_cp es la cota inferior del número de puntos de cambio
#'   que puede tener un cromosoma
#' @param probs_nuevos_muta0N probabilidades de mutar 0,1,2,...,l hasta cierto
#'   numero l; eg si vale c(.5,.2,.2,.1) se tiene una probabilidad 0.5 de mutar
#'   0 (de no mutar), probabilidad 0.2 de mutar 1,, probabilidad 0.2 de mutar 2,
#'   y, probabilidad 0.1 de mutar 3.
#' @param rf_type toma valores en c("W","EW","GGO","MO","GO") y es el nombre de
#'   la función de tasa del NHPP
#' @param initial_val_optim valores iniciales de busqueda del MAP para los
#'   parámetros del NHPP
#' @param mat_low_upp rango de busqueda de los parámetros para el MAP
#' @param vec_dist_a_priori vector de los nobmres de las distribuciones a priori
#'   que se utilizan; eg c("Gamma","Gamma") y c("Gamma","Gamma","Gamma")
#' @param mat_phi matriz cuyos renglones tiene los parámetros de las
#'   distribuciones a priori; cada renglón tiene todos los parametros de una
#'   distribución
#' @param value_set_seed valor de la semilla que se utiliza al correr el
#'   algoritmo genético; puede valer un número (en cuyo caso tomaremos esta
#'   semilla) o puede valer NULL (en cuyo caso se tomará la semilla existente)
#' @param ajuste_bloque en caso de ser TRUE se utiliza el ajuste por bloque de
#'   NHPP para cada bloque del ajuste; ie, siempre se ajusta como primer bloque
#'   y se toma en cuenta en que pedazo se empieza
#' @param print_progress_bar print the progress bar
#' @param print_progress_plots print of the plots of the progress of the algorithm
#'
#' @return regresa un mensaje en caso de que no sea de la longitud correcta el
#'   vector
#' @export
#'
#' @examples
#' revisor_param(param)
revisor_param <- function(param,
                          nombre_datos = c("ciclones", "O3", "O3_U61", "PM10", "PM10_U100")[5],
                          frecuencia_datos = 7, # e.g., 7 equivale a semanal
                          n_datos = c("TODOS", 403, 1003)[1], # tomamos todos o un subconjuto de datos
                          diarios0_rebases1 = 0,
                          valor_de_rebase = 100,
                          r = 50, # número de generaciones 1000
                          k = 50, # tamaño de generaciones 200
                          penalty = c("MDL", "BMDL")[2],
                          max_num_cp = 40, #  = 30
                          prob_inicial = 0.01, # = 0.05 bueno, = 0.2
                          # dist = c("log_norm","poisson","DIC log_norm","DIC poisson")[1],  # cambiar para [2]
                          prob_volado = .6, # = .6
                          probs_muta = c(.4, .2, .4),
                          mutaciones = c(-1, 0, 1),
                          dist_extremos = 10,
                          prob_para_sin_cp = 0.5,
                          cp_real = "sin cp_real",
                          quita_ini0_fin1 = 0, #
                          probs_rank0_MDL1 = 0,
                          # p_m = 0.2, # ya no se ocupa
                          nombre_carpeta_pdf = "Figures",
                          nombre_carpeta_RData = "Data",
                          cuantos_mejores_cp_graf = 100, # cuantos de los mejores cp se graficarán
                          my_data = list(nulo = NULL, "my_data=my_data")[[1]], # se comentó la variable my_data
                          minimo_numero_de_cp = 5, # minimo número de puntos de cambio
                          probs_nuevos_muta0N = c(.5, .2, .2, .1), # probabilidades de mutar 0,1,2,... hasta cierto numero
                          rf_type = c("W", "EW", "GGO", "MO", "GO")[1], # función de tasa de NHPP
                          initial_val_optim = c(.1, .5), # valores iniciales de busqueda del MAP
                          mat_low_upp = matrix(c(c(1e-4, 1e-8), c(1e+1, 1e+5)), nrow = 2), # rango de busqueda de MAP
                          vec_dist_a_priori = c("Gamma", "Gamma"), # distribuciones a priori
                          mat_phi = matrix(c(1, 3, 2, 1.2), ncol = 2),
                          ajuste_bloque = T,
                          print_progress_bar = T,
                          print_progress_plots = T,
                          value_set_seed = 123) { # parametros de dist a priori, cada renglon corresponde a una dist parametro

  # En esta versión se tienen 31 variables 1 oct 2019
  # This version has 33 variables Jan 7 2020
  my_data <- 123
  ejemplo_param <- list(
    nombre_datos = c("ciclones", "O3", "O3_U61", "PM10", "PM10_U100")[5],
    frecuencia_datos = 7, # e.g., 7 equivale a semanal
    n_datos = c("TODOS", 403, 1003)[1], # tomamos todos o un subconjuto de datos
    diarios0_rebases1 = 0,
    valor_de_rebase = 100,
    r = 50, # número de generaciones 1000
    k = 50, # tamaño de generaciones 200
    penalty = c("MDL", "BMDL")[2],
    max_num_cp = 40, #  = 30
    prob_inicial = 0.01, # = 0.05 bueno, = 0.2
    # dist = c("log_norm","poisson","DIC log_norm","DIC poisson")[1],  # cambiar para [2]
    prob_volado = .6, # = .6
    probs_muta = c(.4, .2, .4),
    mutaciones = c(-1, 0, 1),
    dist_extremos = 10,
    prob_para_sin_cp = 0.5,
    cp_real = "sin cp_real",
    quita_ini0_fin1 = 0, #
    probs_rank0_MDL1 = 0,
    # p_m = 0.2, #ya no se ocupa
    nombre_carpeta_pdf = "Figures",
    nombre_carpeta_RData = "Data",
    cuantos_mejores_cp_graf = 100, # cuantos de los mejores cp se graficarán
    my_data = list(nulo = NULL, my_data = my_data)[[1]],
    minimo_numero_de_cp = 5, # minimo número de puntos de cambio
    probs_nuevos_muta0N = c(.5, .2, .2, .1), # probabilidades de mutar 0,1,2,... hasta cierto numero
    rf_type = c("W", "EW", "GGO", "MO", "GO")[1], # función de tasa de NHPP
    initial_val_optim = c(.1, .5), # valores iniciales de busqueda del MAP
    mat_low_upp = matrix(c(c(1e-4, 1e-8), c(1e+1, 1e+5)), nrow = 2), # rango de busqueda de MAP
    vec_dist_a_priori = c("Gamma", "Gamma"), # distribuciones a priori
    mat_phi = matrix(c(1, 3, 2, 1.2), ncol = 2), # parametros de dist a priori, cada renglon corresponde a una dist parametro
    ajuste_bloque = T,
    print_progress_bar = T,
    print_progress_plots = T,
    value_set_seed = 123
  )

  error1_bien0 <- 0
  if (length(param) != length(ejemplo_param)) {
    cat(
      "ERROR in the number of variables of 'param':\nlength(param)=", length(param),
      " and should be ", length(ejemplo_param), "\n"
    )
    cat(
      "It lacks the variables:\n\t", setdiff(names(ejemplo_param), names(param)), "\n",
      "and it has the not needed variables:\n\t", setdiff(names(param), names(ejemplo_param)), "\n"
    )
    error1_bien0 <- 1
  }

  # Validador de la dimensiones de cosas de distribucion a priori
  if (param$rf_type %in% c("W", "MO", "GO")) dimension_priori <- 2
  if (param$rf_type %in% c("EW", "GGO")) dimension_priori <- 3
  if (length(param$vec_dist_a_priori) != dimension_priori) {
    cat(
      "Problemas con la dimension de param$vec_dist_a_priori:\n",
      "\t No tiene dimension ", dimension_priori, " como lo requiere la distribucion ", param$rf_type, "\n"
    )
    error1_bien0 <- 1
  }
  if (nrow(param$mat_phi) != dimension_priori) {
    cat(
      "Problemas con el numero de renglones de param$mat_phi: \n",
      "\t No son ", dimension_priori, " como lo requiere la distribucion ", param$rf_type, "\n"
    )
    error1_bien0 <- 1
  }
  if (nrow(param$mat_low_upp) != dimension_priori) {
    cat(
      "Problemas con el numero de renglones de param$mat_low_upp:\n",
      "\t No son ", dimension_priori, " como lo requiere la distribucion ", param$rf_type, "\n"
    )
    error1_bien0 <- 1
  }

  if (length(param$initial_val_optim) != dimension_priori) {
    cat(
      "Problemas con la longitud de param$initial_val_optim:\n",
      "\t No tiene longitud ", dimension_priori, " como lo requiere la distribucion ", param$rf_type, "\n"
    )
    error1_bien0 <- 1
  }
  # Validador de entradas de mat_low_upp
  if (!all(param$mat_low_upp[, 2] - param$mat_low_upp[, 1] > 0)) {
    cat(
      "La matriz param$mat_low_upp es incorrecta; algunos valores low son",
      "mayores que contraparte upp\n"
    )
  }


  if (param$rf_type == "EW") {
    cat("Recuerda que no se tiene programada la tasa NHPP EW\n")
    error1_bien0 <- 1
  }
  if (error1_bien0 == 1) {
    cat("EXISTEN ERRORES EN param.\n")
  } else {
    cat("The list 'param' is a valid.\n")
  }
}





#' Algoritmo genético de Bayesian MDL a un paso
#'
#' @param x a vector
#' @param mat_cp a matrix
#' @param param
#'
#' @return A `list` of length 2
#' @export
#'
AG_BMDL_1_paso <- function(x, mat_cp, param) {
  # N <- length(x) # ANTES
  N <- max(x)
  # 1. Evaluación de sus calificaciones
  (vec_BMDL_k_cp <- Bayesaian_MDL_k_cp(
    mat_cp, x, param$rf_type,
    param$initial_val_optim,
    param$mat_low_upp,
    param$vec_dist_a_priori,
    param$mat_phi,
    param$ajuste_bloque
  ))
  # 2. Encontrar sus probabilidades
  (vec_probs <- probs_vec_MDL(vec_BMDL_k_cp, param$probs_rank0_MDL1))
  # 3. Seleccionar dos padres
  (mat_padres <- selec_k_pares_de_padres(vec_probs))
  # 4. Juntar sus puntos de cambio
  (mat_cp <- junta_k_puntos_cambio(mat_padres, mat_cp))
  # 5. Volados para quitar puntos de cambio
  (mat_cp <- mata_k_tau_volado(mat_cp, param$prob_volado))
  # 6. Mutaciones puntos de cambio
  (mat_cp <- muta_k_cp_BMDL(mat_cp, x, param))
  # (mat_cp <- muta_k_cp(mat_cp,param)) # antes


  # POR AHORA QUITE LA GENERACIÓN DE NUEVOS PUNTOS DE CAMBIO
  # 7. Genera nuevos puntos de cambio
  # mat_cp <- muta_k_nuevos(mat_cp, param$max_num_cp, N, param$p_m)

  # 8. Regresa el resultado
  return(list(mat_cp = mat_cp, vec_BMDL_k_cp = vec_BMDL_k_cp))
}




#' Algoritmo genético con  Bayesian-MDL
#'
#' @param param
#' @export
AG_BMDL_r_paso <- function(param) {
  # if(!is.null(param$value_set_seed))
  #  set.seed(param$value_set_seed)
  # Primero obtenemos el vector de la variable cruda
  source("Datos.R")
  eval(parse(text = paste0("x <- ", param$nombre_datos)))
  N <- max(x)


  # 1. Simular puntos de cambio iniciales
  mat_cp <- sim_k_cp_BMDL(x, param)
  # Inicializamos lista_AG_BMDL (de esta manero lo podemos meter en el for)
  lista_AG_BMDL <- list(mat_cp = mat_cp)
  # historia_mejores guarda los mejores cp de cada generación
  historia_mejores <- matrix(0, param$r, param$max_num_cp)
  # vec_min_BMDL guarda los valores mínimos del MDL de cada generación
  vec_min_BMDL <- rep(0, param$r)
  # El siguiete for va sobre en cada paso haciendo una nueva generación
  vec_para_for <- 1:param$r
  pb <- utils::txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  graphics::par(mfrow = c(2, 1), mar = c(1, 4, 2, 2))
  for (i in vec_para_for) {
    # Hacemos un paso del AG con el mat_cp anterior
    lista_AG_BMDL <- AG_BMDL_1_paso(x, lista_AG_BMDL$mat_cp, param)
    # Obtenemos el índice del mínimo
    (i_min_BMDL <- which.min(lista_AG_BMDL$vec_BMDL_k_cp))
    # Guardamos el cromosoma mínimo
    historia_mejores[i, ] <- lista_AG_BMDL$mat_cp[i_min_BMDL, ]
    # Guardamos el BMDL del cromosoma mínimo
    vec_min_BMDL[i] <- lista_AG_BMDL$vec_BMDL_k_cp[i_min_BMDL]
    # Imprimimos el porcentaje de progreso

    if (param$print_progress_bar) {
      utils::setTxtProgressBar(pb, i)
    }
    # # Graficamos el valor de BMDL
    # plot(lista_AG$vec_min_BMDL,
    #      xlim = c(1,param$r),type="l",col="blue",ylab="BMDL",xlab="Generation",
    #      main=paste0("AG rate ",param$rf_type," and priori ",paste(param$vec_dist_a_priori,collapse = "-")))
    plot(vec_min_BMDL[1:i],
      xlim = c(1, param$r), type = "l", col = "blue", ylab = "BMDL", xlab = "Generation",
      main = paste0("AG rate ", param$rf_type, " and priori ", fun_n_genera_texto_dist(param))
    )
    # Graficamos los puntos de cambio que más se repitieron
    historia_mejores_sin_0_1_N <- historia_mejores[1:i, -1:-2]
    historia_mejores_sin_0_1_N <- historia_mejores_sin_0_1_N[historia_mejores_sin_0_1_N > 0 & historia_mejores_sin_0_1_N < N]
    plot(table(historia_mejores_sin_0_1_N) / param$r,
      main = "Repeated change points", ylab = "repetitions", xlab = "change points index"
    )
  }
  close(pb)
  graphics::par(mfrow = c(1, 1))
  cat(" \n")

  # Obtenemos el valor minimo de todas las evaluaciones
  valor_BMDL_minimo <- paste0("_BMDL_", floor(min(vec_min_BMDL)))
  minimo_BMDL <- min(vec_min_BMDL)
  where_minimo_BMDL <- which.min(vec_min_BMDL)
  cromosoma_minimo_BMDL <- historia_mejores[where_minimo_BMDL, ]
  # lista_AG contiene los resultados del algoritmo genético
  lista_AG <- list(x = x, historia_mejores = historia_mejores, lista_AG_BMDL = lista_AG_BMDL, vec_min_BMDL = vec_min_BMDL, valor_BMDL_minimo = valor_BMDL_minimo, cromosoma_minimo_BMDL = cromosoma_minimo_BMDL, minimo_BMDL = minimo_BMDL, param = param)

  graf_puntos_cambio_repetidos(lista_AG)


  graficas_BMDL(lista_AG, param)

  nombre_pdf <- paste0(
    param$nombre_carpeta_pdf, "/Fig_4AGBMDL_", param$nombre_datos, "_rf_",
    param$rf_type, "_", fun_n_genera_texto_dist(param), "_r",
    param$r, "_k",
    param$k, lista_AG$valor_BMDL_minimo, ".pdf"
  )

  grDevices::dev.print(pdf, nombre_pdf, width = 16, height = 10)
  cat("Se guardo la imagen:\n", nombre_pdf, "\n")

  nombre_archivo_AG <- paste0(
    param$nombre_carpeta_RData, "/Dat_AGBMDL_", param$nombre_datos, "_rf_",
    param$rf_type, "_", fun_n_genera_texto_dist(param), "_r",
    param$r, "_k",
    param$k, lista_AG$valor_BMDL_minimo, ".RData"
  )

  save(lista_AG, file = nombre_archivo_AG)
  cat("Se guardo el archivo:\n", nombre_archivo_AG, "\n")

  return(lista_AG)
}




#' Bayesian MDL para un vector de puntos de cambio
#'
#' @param cp
#' @param x
#' @param rf_type
#' @param initial_val_optim
#' @param mat_low_upp
#' @param vec_dist_a_priori
#' @param mat_phi
#' @param ajuste_bloque description
#' @export
Bayesaian_MDL_1_cp <- function(cp, x, rf_type, initial_val_optim, mat_low_upp, vec_dist_a_priori, mat_phi, ajuste_bloque) {
  # 1. Obtener los estimadores MAP para cada regimen y guardarlos en mat_MAP
  mat_MAP <- extrae_mat_MAP(cp, x, rf_type, initial_val_optim, mat_low_upp, vec_dist_a_priori, mat_phi, ajuste_bloque)
  # 2. Evaluar la log-posterior (sumando la primera columna de mat_MAP)
  log_posterior <- sum(mat_MAP[, 1])
  # 3. Evaluar la penalización
  penaliza_cp <- penalization_MDL(cp, rf_type)
  # 4. Obtener bayesian-MDL de la diferencia de la penalización y la log-posterior
  (BMDL_1_cp <- penaliza_cp - log_posterior)
  return(BMDL_1_cp)
}

#' Bayesian MDL para un vector de puntos de cambio
#'
#' @param x
#' @param rf_type
#' @param initial_val_optim
#' @param mat_low_upp
#' @param vec_dist_a_priori
#' @param mat_phi
#'
#' @return regresa un vector de tamaño k (el numero de cromosomas por
#'   generación) con los valores del bayesian MDL
#' @export
Bayesaian_MDL_k_cp <- function(mat_cp, x, rf_type, initial_val_optim, mat_low_upp, vec_dist_a_priori, mat_phi, ajuste_bloque) {
  # OBS: quizás se podría hacer matricial para que fuera más rápido
  return(apply(mat_cp, 1, function(y) {
    Bayesaian_MDL_1_cp(y, x, rf_type, initial_val_optim, mat_low_upp, vec_dist_a_priori, mat_phi, ajuste_bloque)
  }))
}



#' Bloque de log posterior NHPP
#'
#'
#' @param vec_d_i vector de días en los que hubo revases entre el los puntos de
#'   cambio tau1 y tau2
#' @param tau1 valor del primer punto de cambio
#' @param tau2 valor del segundo punto de cambio
#' @param rf_type nombre de tasa de NHPP
#' @param theta vector de parámetros de verosimilitud del NHPP
#' @param vec_dist_a_priori
#' @param mat_phi
#' @export
Bloq_LogPost_NHPP <- function(vec_d_i, tau1, tau2, rf_type, theta, vec_dist_a_priori, mat_phi) {
  Bloq_LogVero_NHPP(vec_d_i, tau1, tau2, rf_type, theta) +
    Bloq_LogPrio_NHPP(vec_dist_a_priori, theta, mat_phi)
}



#' Bloque de log-a priori NHPP
#'
#' @param vec_dist_a_priori
#' @param theta
#' @param mat_phi
#' @export
Bloq_LogPrio_NHPP <- function(vec_dist_a_priori, theta, mat_phi) {
  if (length(vec_dist_a_priori) == 2) {
    if (all(vec_dist_a_priori == c("Gamma", "Gamma"))) {
      return((mat_phi[1, 2] - 1) * log(theta[1]) - mat_phi[1, 1] * theta[1] + # Exp 74 pag 21
        (mat_phi[2, 2] - 1) * log(theta[2]) - mat_phi[2, 1] * theta[2]) # Exp 75 pag 21
    }
  } else if (length(vec_dist_a_priori) == 3) {
    if (all(vec_dist_a_priori == c("Gamma", "Gamma", "Gamma"))) {
      return((mat_phi[1, 2] - 1) * log(theta[1]) - mat_phi[1, 1] * theta[1] +
        (mat_phi[2, 2] - 1) * log(theta[2]) - mat_phi[2, 1] * theta[2] +
        (mat_phi[3, 2] - 1) * log(theta[3]) - mat_phi[3, 1] * theta[3]) # Antes de la exp 76 pag 22
    }
  } else {
    print("No se tiene registrada esa vec_dist_a_priori; Bloq_LogPrio_NHPP")
  }
}




#' Bloque de log-verosimilitud NHPP
#'
#' @param vec_d_i vector de días en los que hubo rebases entre el los puntos de
#'   cambio tau1 y tau2
#' @param tau1 valor del primer punto de cambio
#' @param tau2 valor del segundo punto de cambio
#' @param rf_type nombre de tasa de NHPP
#' @param theta vector de parámetros de verosimilitud del NHPP
#' @export
Bloq_LogVero_NHPP <- function(vec_d_i, tau1, tau2, rf_type, theta) {
  if (rf_type == "W") {
    return((tau1^theta[1] - tau2^theta[1]) / theta[2]^theta[1] +
      length(vec_d_i) * (log(theta[1]) - theta[1] * log(theta[2])) +
      (theta[1] - 1) * sum(log(vec_d_i)))
  } # Expresion pg 15 entre las expresiones 18 y 19
  if (rf_type == "EW") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    sigma <- theta[3]
    sumd <- sum(vec_d_i)
    sumlogd <- sum(log(vec_d_i))
    sumlog1menosed <- sum(log(1 - exp(-(vec_d_i / sigma)^alpha))) # exp 44 pg 18
    sumlog1menosed2 <- sum(log(1 - (1 - exp(-(vec_d_i / sigma)^alpha))^beta)) # exp 46 pg 18
    return(-sigma^(-alpha) * sumd^alpha # exp 42 pg 18
      + (-1 + beta) * sumlog1menosed - sumlog1menosed2 + # exp 44 pg 18
      (-1 + alpha) * sumlogd + difN * log(alpha * beta) - log(1 - (1 - exp(-(tau1 / sigma)^alpha))^beta) + # exp 43_1+exp 42_1-exp 30 pg 16
      log(1 - (1 - exp(-(tau2 / sigma)^alpha))^beta) - (-1 + alpha) * difN * log(sigma)) # exp 46-exp 43_2
  }
  if (rf_type == "GO") {
    return(theta[1] * (exp(-theta[2] * tau2) - exp(-theta[2] * tau1)) + length(vec_d_i) * (log(theta[1]) + log(theta[2])) - theta[2] * sum(vec_d_i)) # exp 54 pg 19
  }
  if (rf_type == "GGO") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    sigma <- theta[3]
    # sumd <- sum(vec_d_i)
    sumlogd <- sum(log(vec_d_i))
    sumdasigma <- sum(vec_d_i^sigma)
    return(alpha * (exp(-beta * tau2^sigma) - exp(-beta * tau1^sigma)) + (-1 + sigma) * sumlogd + difN * (log(alpha) + log(beta) + log(sigma)) - beta * sumdasigma) # exp 63 pg 20
  }
  if (rf_type == "MO") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    sumlogalphamasd <- sum(log(alpha + vec_d_i))
    return(difN * log(beta) + beta * (log(alpha + tau1) - log(alpha + tau2)) - sumlogalphamasd) # exp 71 pg 21
  }
}




#' Derivada bloque de log posterior NHPP
#'
#' @param vec_d_i
#' @param tau1
#' @param tau2
#' @param rf_type
#' @param theta
#' @param vec_dist_a_priori
#' @param mat_phi
#' @export
D_Bloq_LogPost_NHPP <- function(vec_d_i, tau1, tau2, rf_type, theta, vec_dist_a_priori, mat_phi) {
  D_Bloq_LogVero_NHPP(vec_d_i, tau1, tau2, rf_type, theta) +
    D_Bloq_LogPrio_NHPP(vec_dist_a_priori, theta, mat_phi)
}



#' Derivada de bloque de log-a priori NHPP
#'
#' @param vec_dist_a_priori vector que determina cual es la distribución a
#'   priori de los parametros; por ahora se tiene programado
#'   vec_dist_a_priori=c("Gamma","Gamma") y
#'   vec_dist_a_priori=c("Gamma","Gamma","Gamma")
#' @param theta vector de parámetros de verosimilitud
#' @param mat_phi matriz cuyos renglones tiene los parámetros de las
#'   distribuciones a priori; cada renglón tiene todos los parametros de una
#'   distribución
#'
#' @export
D_Bloq_LogPrio_NHPP <- function(vec_dist_a_priori, theta, mat_phi) {
  if (length(vec_dist_a_priori) == 2) {
    if (all(vec_dist_a_priori == c("Gamma", "Gamma"))) {
      # Parcial con respecto a alfa
      p1 <- (-1 - theta[1] * mat_phi[1, 1] + mat_phi[1, 2]) / theta[1]
      # Parcial con respecto a beta
      p2 <- (-1 - theta[2] * mat_phi[2, 1] + mat_phi[2, 2]) / theta[2]
      return(c(p1, p2))
    }
  } else if (length(vec_dist_a_priori) == 3) {
    if (all(vec_dist_a_priori == c("Gamma", "Gamma", "Gamma"))) {
      # Parcial con respecto a alfa
      p1 <- (-1 - theta[1] * mat_phi[1, 1] + mat_phi[1, 2]) / theta[1]
      # Parcial con respecto a beta
      p2 <- (-1 - theta[2] * mat_phi[2, 1] + mat_phi[2, 2]) / theta[2]
      # Parcial con respecto a beta
      p3 <- (-1 - theta[3] * mat_phi[3, 1] + mat_phi[3, 2]) / theta[3]
      return(c(p1, p2, p3))
    }
  } else {
    print("No se tiene registrada esa vec_dist_a_priori; D_Bloq_LogPrio_NHPP")
  }
}





#' Derivadas de bloque de log-verosimilitud NHPP
#'
#' @param vec_d_i vector de días en los que hubo revases entre el los puntos de
#'   cambio tau1 y tau2
#' @param tau1 valor del primer punto de cambio
#' @param tau2 valor del segundo punto de cambio
#' @param rf_type nombre de tasa de NHPP
#' @param theta vector de parámetros de verosimilitud del NHPP
#' @export
D_Bloq_LogVero_NHPP <- function(vec_d_i, tau1, tau2, rf_type, theta) {
  if (rf_type == "W") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    sumlogd <- sum(log(vec_d_i))
    if (tau1 == 0) { # este es el caso del primer regimen
      # Parcial de alpha
      p1 <- difN / alpha + sumlogd - difN * log(beta) + beta^(-alpha) * tau2^alpha * (log(beta) - log(tau2))
      # Parcial de beta
      p2 <- alpha * beta^(-1 - alpha) * (-difN * beta^alpha + tau2^alpha)
    } else { # para los otros regímenes (o bloques)
      # Parcial de alpha
      p1 <- difN / alpha + sumlogd + beta^(-alpha) * (-(difN * beta^alpha + tau1^alpha - tau2^alpha) * log(beta) + tau1^alpha * log(tau1) - tau2^alpha * log(tau2))
      # Parcial de beta
      p2 <- -alpha * beta^(-1 - alpha) * (difN * beta^alpha + tau1^alpha - tau2^alpha)
    }
  }



  if (rf_type == "EW") {
    return("Me niego a hacer esta, es muy larga; D_Bloq_LogVero_NHPP")
  }
  if (rf_type == "GO") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    sumd <- sum(vec_d_i)

    if (tau1 == 0) { # este es el caso del primer regimen
      # Parcial con respecto de alpha
      p1 <- difN / alpha - exp(-beta * tau1) + exp(-beta * tau2)
      # Parcial con respecto de beta
      p2 <- difN / beta - sumd - alpha * exp(-beta * tau2) * tau2
    } else {
      # Parcial con respecto de alpha
      p1 <- difN / alpha - exp(-beta * tau1) + exp(-beta * tau2)
      # Parcial con respecto de beta
      p2 <- difN / beta - sumd + alpha * exp(-beta * tau1) * tau1 - alpha * exp(-beta * tau2) * tau2
    }
  }
  if (rf_type == "GGO") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    sigma <- theta[3]
    # sumd <- sum(vec_d_i)
    sumlogd <- sum(log(vec_d_i))
    sumdasigma <- sum(vec_d_i^sigma)
    D_sumdasigma <- sum(log(vec_d_i) * vec_d_i^sigma)
    if (tau1 == 0) { # este es el caso del primer regimen
      # Parcial con respecto de alpha
      p1 <- -1 + difN / alpha + exp(-beta * tau2^sigma)
      # Parcial con respecto de beta
      p2 <- difN / beta - alpha * exp(-beta * tau2^sigma) * tau2^sigma - sumdasigma
      # Parcial con respecto de sigma
      p3 <- difN / sigma + sumlogd - alpha * beta * exp(-beta * tau2^sigma) * tau2^sigma * log(tau2) - beta * D_sumdasigma
    } else {
      # Parcial con respecto de alpha
      p1 <- difN / alpha - exp(-beta * tau1^sigma) + exp(-beta * tau2^sigma)
      # Parcial con respecto de beta
      p2 <- difN / beta + alpha * exp(-beta * tau1^sigma) * tau1^sigma - alpha * exp(-beta * tau2^sigma) * tau2^sigma - sumdasigma
      # Parcial con respecto de sigma
      p3 <- difN / sigma + sumlogd + alpha * beta * exp(-beta * tau1^sigma) * tau1^sigma * log(tau1) - alpha * beta * exp(-beta * tau2^sigma) * tau2^sigma * log(tau2) - beta * D_sumdasigma
    }
  }
  if (rf_type == "MO") {
    difN <- length(vec_d_i)
    alpha <- theta[1]
    beta <- theta[2]
    D_sumlogalphamasd <- sum(1 / (alpha + vec_d_i))
    # OBS: los resultadso tau1==0 y tau1!=0 son iguales, se podría borrar el if
    if (tau1 == 0) { # este es el caso del primer regimen
      # Parcial con respecto de alpha
      p1 <- (beta * tau2) / (alpha^2 + alpha * tau2) - D_sumlogalphamasd
      # Parcial con respecto de beta
      p2 <- difN / beta + log(alpha) - log(alpha + tau2)
    } else {
      # Parcial con respecto de alpha
      p1 <- beta * (1 / (alpha + tau1) - 1 / (alpha + tau2)) - D_sumlogalphamasd
      # Parcial con respecto de beta
      p2 <- difN / beta + log(alpha + tau1) - log(alpha + tau2)
    }
  }
  if (rf_type %in% c("W", "GO", "MO")) {
    return(c(p1, p2))
  } else {
    return(c(p1, p2, p3))
  }
}


#' Extrae matriz con estimadores MAP
#'
#' @param rf_type nombre de tasa de NHPP
#' @param vec_dist_a_priori nombres de distribuciones a priori
#' @param mat_phi matriz cuyos renglones tiene los parámetros de las
#'   distribuciones a priori
#' @param mat_low_upp matriz con lugares donde buscar; cada renglon es para un
#'   parámetro del NHPP
#' @param initial_val_optim valores iniciales que utiliza la función optim para
#'   encontrar el mínimo
#' @param cp vector de tamaño max_num_cp con entradas m, tau_0=1 , ...,
#'   tau_{m+1}, 0, ..., 0
#' @param x
#'
#' @return regresa una matriz cuya primera columna es la log-posterior evaluada
#'   en los estimadores MAP; sus siguientes columnas tiene los parametros de
#'   cada regimen.
#'
#' @export
extrae_mat_MAP <- function(cp, x, rf_type, initial_val_optim, mat_low_upp, vec_dist_a_priori, mat_phi, ajuste_bloque) {
  # lista_insumos_bloque <- genera_insumos_bloque(cp,x,theta_mat) ANTES
  lista_insumos_bloque <- genera_insumos_bloque_sin_theta(cp, x)
  n_mle <- cp[1] + 1

  if (param$rf_type %in% c("W", "MO", "GO")) dimension_priori <- 2
  if (param$rf_type %in% c("EW", "GGO")) dimension_priori <- 3

  mat_MAP <- matrix(0, n_mle, dimension_priori + 1)

  colnames(mat_MAP) <- c("log-posterior", "alpha", "beta", "sigma")[1:(dimension_priori + 1)]

  # El siguiente for va sobre cada
  for (i in 1:n_mle) {
    # MAP_NHPP(initial_val,mat_low_upp,vec_d_i,tau1,tau2,rf_type,vec_dist_a_priori,mat_phi){
    aux_map <- MAP_NHPP(
      initial_val_optim, mat_low_upp, lista_insumos_bloque$lista_dias_regimen[[i]],
      lista_insumos_bloque$mat_tau[i, 1],
      lista_insumos_bloque$mat_tau[i, 2], rf_type, vec_dist_a_priori, mat_phi
    )
    # Obs: MAP_NHPP regresa la menos-log-posterior, por eso la multiplicamos por menos
    mat_MAP[i, ] <- c(-aux_map$value, aux_map$par)
  }
  return(mat_MAP)
}


#' Genera los textos para los pdf y RData de las n distribuciones
#'
#' @param param
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
#'
#' @param dist
#' @param parametros_dist
#' @export
fun_1_genera_texto_dist <- function(dist, parametros_dist) {
  if (any(dist == c("Gamma", "Unif"))) {
    texto <- paste0(dist, "(", parametros_dist[1], ",", parametros_dist[2], ")")
  } else {
    for (i in 1:10) print("no se tiene esta distribución; fun_1_genera_texto_dist")
  }
  return(texto)
}



#' Grafica los n puntos de cambio más repetidos sobre el plot.stepfun de los datos
#'
#' @param lista_AG
#'
#' @return guarda un pdf con la gráfica
#' @export
graf_puntos_cambio_repetidos <- function(lista_AG) {
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


  (nombre_pdf <- paste0(
    param$nombre_carpeta_pdf, "/Fig_CP_repetidos_", lista_AG$param$nombre_datos, "_rf_",
    lista_AG$param$rf_type, "_", fun_n_genera_texto_dist(lista_AG$param), "_r",
    lista_AG$param$r, "_k",
    lista_AG$param$k, lista_AG$valor_BMDL_minimo, ".pdf"
  ))

  grDevices::dev.print(pdf, nombre_pdf, width = 16, height = 10)
  cat("Se guardo la imagen:\n", nombre_pdf, "\n")
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

#' pow
#'
#' @param n_puntos_cambio numero de puntos de cambio
#' @param mas_derecha es una variable que fue utilizada para que
#'  pudiera ejecutarse en R y en JAGS; toma los valores "" (vacío) y "+".
#'
#' @return Regresa el texto correspondiente a la variable m
#' @export
#'

pow <- function(x, y) {
  x^y
}
#' step
#'
#' @param n_puntos_cambio numero de puntos de cambio
#' @param mas_derecha es una variable que fue utilizada para que
#'  pudiera ejecutarse en R y en JAGS; toma los valores "" (vacío) y "+".
#'
#' @return Regresa el texto correspondiente a la variable m
#' @export
#'

step <- function(x) {
  as.numeric(x > 0)
}


#' print_pow_tau
#'
#' @param n_puntos_cambio numero de puntos de cambio
#' @param mas_derecha es una variable que fue utilizada para que
#'  pudiera ejecutarse en R y en JAGS; toma los valores "" (vacío) y "+".
#'
#' @return Regresa el texto correspondiente a la variable m
#' @export
#'
print_pow_tau <- function(i_tau, i_sig_alp) {
  paste0("pow(tau[", i_tau, "]/sigma[", i_sig_alp, "],alpha[", i_sig_alp, "])")
}

#' print_pow_d
#'
#' @param n_puntos_cambio numero de puntos de cambio
#' @param mas_derecha es una variable que fue utilizada para que
#'  pudiera ejecutarse en R y en JAGS; toma los valores "" (vacío) y "+".
#'
#' @return Regresa el texto correspondiente a la variable m
#' @export
#'

print_pow_d <- function(i_d, i_sig_alp) {
  paste0("pow(d[", i_d, "]/sigma[", i_sig_alp, "],alpha[", i_sig_alp, "])")
}

print_step <- function(i_d, i_tau, extra_menos = F, Do_step2 = F) {
  if (Do_step2) {
    if (!extra_menos) texto <- paste0("step2(d[", i_d, "]-tau[", i_tau, "]-0.5)")
    if (extra_menos) texto <- paste0("step2(-(d[", i_d, "]-tau[", i_tau, "]-0.5))")
  } else {
    if (!extra_menos) texto <- paste0("step(d[", i_d, "]-tau[", i_tau, "]-0.5)")
    if (extra_menos) texto <- paste0("step(-(d[", i_d, "]-tau[", i_tau, "]-0.5))")
  }
  return(texto)
}



#' gen_texto_m
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
gen_texto_m <- function(n_puntos_cambio, mas_derecha = "") {
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
  texto_m <- paste0("funcion_media_acumulada<-function(i){", texto_m)
  texto_m <- paste0(texto_m, "}")

  nombre_archivo <- paste0("funcion_media_acumulada.R")
  cat(texto_m, file = nombre_archivo)
  # cat("Se genero el archivo: ",nombre_archivo)
}



#' Graficas de los intervalos y los datos
#'
#' @param lista_AG
#' @param param
#' @export
grafica_datos_e_intervalos <- function(lista_AG, param) {
  # sink("funcion_media_acumulada.txt")
  gen_texto_m(n_puntos_cambio, mas_derecha = "")
  # sink()
  source("funcion_media_acumulada.R")
  grafica_escalonada(lista_AG$x, "black")

  tasa_NHPP <- funcion_media_acumulada()
  upp_bond <- stats::qpois(.95, lambda = c(pow(10 / sigma[1], alpha[1]), tasa_NHPP))
  low_bond <- stats::qpois(.05, lambda = c(pow(10 / sigma[1], alpha[1]), tasa_NHPP))
  mean_NHPP <- c(pow(10 / sigma[1], alpha[1]), tasa_NHPP)
  graphics::lines(c(10, d), upp_bond, col = "blue", lwd = 2)
  graphics::lines(c(10, d), mean_NHPP, col = "red", lwd = 2)
  graphics::lines(c(10, d), low_bond, col = "blue", lwd = 2)
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


#' Graficas del AG con BMDL
#'
#' @param lista_AG
#' @param param
#'
#' @return una gráfica con :
#' 1 Datos reales
#' 2 Evolución del algoritmo genético
#' 3 Puntos de cambio que más se repitieron
#' 4 Number of change points in the best chromosomes
#'
#' @export
graficas_BMDL <- function(lista_AG, param) {
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
  grafica_datos_e_intervalos(lista_AG, param)

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
}


#' Hace un hijo de dos padres
#'
#' @param padres vector de longitud dos con índice de papa e índice de mama
#' @param mat_cp matriz con tiempos de cambio de dimension k por max_num_cp
#'
#' @export
#'
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


#' Hace k hijos de k parejas de padres
#'
#' @param parejas_padres matriz de kx2 la cual contiene en sus renglones las
#'   parejas de padres
#' @param mat_cp matriz con cromosomas cambio de tamaño max_num_cp con entradas
#'   m,tau_0=1,...,tau_{m+1}=N,0,...,0
#'
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


#' Elimina algunos de las tiempos de cambio de un cromosoma
#'
#' Regresa un vector del mismo tamaño que cp pero despues de eliminar
#' algunas de sus entradas
#' @param cp vector cromosoma que se va a poner a prueba
#' @param prob_volado probabilidad de quitar un tiempo de cambio existente
#'
#' @return el mismo cromosoma sin algunos de sus puntos de cambio
#' @export
mata_1_tau_volado <- function(cp, prob_volado) {
  # (cp <- sim_1_cp(N,param) )
  # (m <- cp[1])
  # N <- cp[cp[1]+3]
  (cp_corto <- cp[2:(cp[1] + 3)])
  (cp_corto <- cp_corto[c(TRUE, as.logical(stats::rbinom(cp[1], 1, prob_volado)), TRUE)])
  cp <- c(length(cp_corto) - 2, cp_corto, rep(0, length(cp) - length(cp_corto) - 1))
  return(cp)
}

#' Elimina algunos de las tiempos de cambio de los k cromosomas
#'
#' @param prob_volado probabilidad de quitar un tiempo de cambio existente
#' @param mat_cp matriz cuyos renglones son vectores de cromosomas de tamaño
#'   max_num_cp con entradas m,tau_0,...,tau_{m+1},0,...,0
#' @return regresa una matriz a la cual se le quitaron a sus cromosomas algunos
#'   puntos de cambio
#' @export
mata_k_tau_volado <- function(mat_cp, prob_volado) {
  for (i in 1:nrow(mat_cp)) {
    mat_cp[i, ] <- mata_1_tau_volado(mat_cp[i, ], prob_volado)
  }
  return(mat_cp)
  # return(apply(mat_cp, 1, function(yy){ mata_1_tau_volado(yy,prob_volado)} ) )
}

#' Evaluanción de mean function (la integral de la rate)
#'
#' @param t valor a evaluar (real)
#' @param rf_type tipo de mean function (ver libro de Eliane pdf 40 eq (3.12));
#'               posibles exponentiated-Weibull (EW),the Musa–Okumoto (MO),
#'               the Goel–Okumoto (GO), and a generalized Goel–Okumoto (GGO)
#' @param theta parámetros de mean function.Para
#'
#'   W: alpha = theta[1], sigma=theta[2]
#'
#'   EW: alpha=theta[1], beta=theta[2] y sigma=theta[3]
#'
#'   GGO: alpha=theta[1], beta=theta[2] y gamma=theta[3]
#'
#'   MO: alpha=theta[1], beta=theta[2]
#'
#'   GO: alpha=theta[1], beta=theta[2]
#'
#' @return regresa la evaluación
#' @export
#'
#' @examples
#' t <- c(1.4, 2.8)
#' theta <- c(1.2, 2.1, 3.2)
#' rf_type <- "EW"
#' mean_fn(t, rf_type, theta)
#' rf_type <- "GGO"
#' mean_fn(t, rf_type, theta)
#' rf_type <- "MO"
#' mean_fn(t, rf_type, theta)
#' rf_type <- "GO"
#' mean_fn(t, rf_type, theta)
#'
mean_fn <- function(t, rf_type, theta) {
  if (rf_type == "W") {
    return((t / theta[2])^theta[1])
  }
  if (rf_type == "EW") {
    return(-log(1 - (1 - exp(-(t / theta[3])^theta[1]))^theta[2]))
  }
  if (rf_type == "GGO") {
    return(theta[1] * (1 - exp(-theta[2] * t^theta[3])))
  }
  if (rf_type == "MO") {
    return(theta[2] * log(1 + t / theta[1]))
  }
  if (rf_type == "GO") {
    return(theta[1] * (1 - exp(-theta[2] * t)))
  } else {
    print("no se tiene esa rf_type; mean_fn")
  }
}

#' Mutaciones un cp en el caso BMDL
#'
#' @param cp puntos de cambio
#' @param x vector de revases
#' @param param parametros
#'
#' @return regresa un vector mutado
#' @export
#'
muta_1_cp_BMDL <- function(cp, x, param) {
  # eval(parse(text=paste0("x <- ",param$nombre_datos)))
  # (cp <- sim_1_cp_BMDL(x,param) )

  # En caso de tener muy pocos puntos de cambio, rehacemos el cp
  if (cp[1] <= param$minimo_numero_de_cp) {
    return(sim_1_cp_BMDL(x, param))
  }

  (cp_posibles_muta <- cp[3:(cp[1] + 2)])
  # Indices mutados
  i_mutados <- sort(unique(sapply(
    match(cp_posibles_muta, x) + sample(param$mutaciones, size = cp[1], prob = param$probs_muta, replace = T),
    function(yy) {
      min(max(yy, param$dist_extremos), length(x) - param$dist_extremos)
    }
  )))
  # Perturbamos los tau
  (cp_posibles_muta <- x[i_mutados])

  # Agregamos algunos puntos de cambio aleatoriamente
  if (length(cp_posibles_muta) < param$max_num_cp - 3) {
    # Simulamos cuantos vamos a agregar
    (cuantos_nuevos <- sample(0:(length(param$probs_nuevos_muta0N) - 1), size = 1, prob = param$probs_nuevos_muta0N))
    # print(cuantos_nuevos)
    if (length(cp_posibles_muta) < param$max_num_cp + cuantos_nuevos && cuantos_nuevos > 0) {
      cp_posibles_muta <- sort(unique(c(
        cp_posibles_muta,
        sample(x[param$dist_extremos:(length(x) - param$dist_extremos)], cuantos_nuevos)
      )))
    }
  }
  # cp_posibles_muta
  # length(cp_posibles_muta)

  # Agregamos las mutaciones a la estructura que utilizamos en los cromosomas
  cp <- c(length(cp_posibles_muta), 1, cp_posibles_muta, cp[cp[1] + 3], rep(0, param$max_num_cp - length(cp_posibles_muta) - 3))
  return(cp)
}


#' Mutaciones un cp en el caso BMDL
#'
#' @param mat_cp
#' @param x
#' @param param
#'
#' @return regreas una mat_cp mutada
#' @export
muta_k_cp_BMDL <- function(mat_cp, x, param) {
  mat_muta <- matrix(0, param$k, param$max_num_cp)
  for (i in 1:param$k) mat_muta[i, ] <- muta_1_cp_BMDL(mat_cp[i, ], x, param)
  return(mat_muta)
}




#' Genera un cromosoma de puntos de cambio para el Bayesian MDL
#'
#' @param x vector de excedentes
#' @param param lista de parámetros globales
#'
#' @details
#' regresa un vector de tamaño `max_num_cp+3` donde la primera entrada es
#'         m, la segunda \eqn{v_0=1, ...., v_{m+1}=N,0,...,0}
#' por ejemplo: `c(4,1,3,8,11,15,20,0,0,0,0)` para `m=4`, \eqn{max_num_cp=8}, \eqn{N=20}.
#'         Se tienen m puntos de cambio, los cuales \eqn{\tau_0=1} y \eqn{\tau_{m+1}= N+1}, pero en nuestro caso
#'         tenemos que los vectores cp tienen \eqn{c(m,\tau_0=1,\tau_1,...,\tau_{m-1},\tau_m= N,0,0,0)}
#'         por lo cual se nosotros:
#' - 1) empieza con el número de puntos de cambio;
#' - 2) la segunda entrada es un uno;
#' - 3) la tercera entrada es el primer punto de cambio;
#' - 4) las siguientes son otros puntos de cambio;
#' - 5) la siguiente entrada después de punto de cambio tiene el valor  `N`; y
#' - 6) los siguientes son númores cero hasta llenarlo para que sea de tamaño `max_num_cp`
#' @export
#'
sim_1_cp_BMDL <- function(x, param) {
  # Primero simulamos una binomial que va a ser el número de puntos de cambio
  (m <- min(stats::rbinom(1, max(x), param$prob_inicial), param$max_num_cp - 3))
  # Simulamos los puntos de cambio uniformemente aleatorios
  (valores_cp <- sort(sample(x[-length(x)], size = m, replace = F)))
  # Genera cromosoma con estructura manejable
  (ans <- c(m, 1, valores_cp, max(x), rep(0, param$max_num_cp - m - 3)))
  return(ans)
}


#' Simula k vectores change point para Bayesian MDL
#'
#' @param x vector de excedentes
#' @param param lista de parámetros globales
#'
#' @return regresa una matriz de k por max_num_cp+3, la cual en cada renglón tiene
#'         una simulación de un vector de tiempos de cambio
#' @export
#'
sim_k_cp_BMDL <- function(x, param) {
  mat_cp <- matrix(0, param$k, param$max_num_cp)
  for (i in 1:param$k) {
    mat_cp[i, ] <- sim_1_cp_BMDL(x, param)
  }
  return(mat_cp)
}




#' Penalización MDL
#'
#' @param cp vector de extendido de puntos de cambio
#'
#' @return regresa la evaluación de la penalización
#'  \deqn{
#'    P_{\theta,\tau} = \sum_{i=1}^{m+1}\dfrac{\ln(\tau_i-\tau_{i-1})}{2}+\ln(m)+\sum_{i=2}^m\ln(\tau_i)
#'  }
#' @export
#'
penalization_MDL <- function(cp, rf_type) { # V02
  # Se hizo el cambio de multiplicar por en número de parámetros
  # esta función solo es llamada por "penalization_MDL"
  # penalization_MDL <- function(cp) { # antes no recibía rf_type

  # n_param_rf_type es el número de parámetros de la función de tasa del poisson
  n_param_rf_type <- c(2, 3, 3, 2, 2)[rf_type == c("W", "EW", "GGO", "MO", "GO")]

  (cp_corto_cero <- c(0, cp[3:(cp[1] + 3)]))
  # En particular se agregó la parte *n_param_rf_type/2
  return(sum(log(cp_corto_cero[-1] - cp_corto_cero[-cp[1] - 2]) * n_param_rf_type / 2) + log(cp[1]) + sum(log(cp_corto_cero[c(-1, -cp[1] - 2)])) + (cp[1] * log(N)))
}



#' Estimadores MAP
#'
#' @param vec_d_i vector de días de un régimen
#' @param tau1 valor del primer punto de cambio
#' @param tau2 valor del segundo punto de cambio
#' @param rf_type nombre de tasa de NHPP
#' @param vec_dist_a_priori nombres de distribuciones a priori
#' @param mat_phi matriz cuyos renglones tiene los parámetros de las
#'   distribuciones a priori
#' @param mat_low_upp matriz con lugares donde buscar; cada renglon es para un
#'   parámetro del NHPP
#' @param initial_val_optim valores iniciales que utiliza la función optim para
#'   encontrar el mínimo
#'
#' @return regresa un el resultado de optim
#' @export
#'
MAP_NHPP <- function(initial_val_optim, mat_low_upp, vec_d_i, tau1, tau2, rf_type, vec_dist_a_priori, mat_phi) {
  # Definimos las funciones que vamos a utilizar para encontrar el mínimo
  my_fn <- function(theta) {
    -Bloq_LogPost_NHPP(vec_d_i, tau1, tau2, rf_type, theta, vec_dist_a_priori, mat_phi)
  }
  my_gn <- function(theta) {
    -D_Bloq_LogPost_NHPP(vec_d_i, tau1, tau2, rf_type, theta, vec_dist_a_priori, mat_phi)
  }
  # Calculamos el mínimo
  (val_optimos <- stats::optim(initial_val_optim,
    fn = my_fn, gr = my_gn,
    lower = mat_low_upp[, 1], upper = mat_low_upp[, 2],
    method = "L-BFGS-B"
  ))
  return(val_optimos)
}



#' Probabilidades a partir de mat_MDL
#'
#' @param vec_MDL vector con valores MDL
#'
#' OBSERVACIÓN: Esto regresa numeros negativos, los cuales mientras más negativo mejor, ya que
#'             dará que es un mejor vector de tiempos de cambio. Es decir, un MDL de -6000 es
#'             mejor que -4000
#' @param probs_rank0_MDL1
#'
#' @return regresa un vector de probabilidades
#' @export
probs_vec_MDL <- function(vec_MDL, probs_rank0_MDL1) {
  if (any(is.infinite(vec_MDL))) {
    print("Valor infinito; fun probs_vec_MDL, vec_MDL=")
    print(vec_MDL)
  }
  if (probs_rank0_MDL1 == 0) {
    return(rank(-vec_MDL))
  }
  if (probs_rank0_MDL1 == 1) {
    return(-vec_MDL)
  }
}


#' Seleciona k pares de padres
#'
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



#' Actualizador de archivos de una carpeta a otra
#'
#' @param comienzos_de_archivo este es el vector de los comienzos de los nombres
#'   de los archivos; eg si comienzos_de_archivo = c("MDL V","Fn AG Bayesian
#'   MDL.R"), entonces se buscarán los archivos que comienzen con "MDL V" y se
#'   copiará el archivo con la terminación mas grande, ie si existen los
#'   archivos "Fn AG Bayesian MDL.R", "MDL V01", "MDL V02", "MDL V03" se copiará
#'   el archivo "Fn AG Bayesian MDL.R" (por ser único) y el archivo "MDL V03"
#'   (por ser el que tiene el nombre mas grande alfanumericamente)
#' @param source_raiz dirección de donde se toman los archivos a copiar
#' @param carpeta_destino dirección a donde se quieren copiar los archivos
#'
#' @export
#'
actualiza_carpeta <- function(comienzos_de_archivo, source_raiz, carpeta_destino) {
  for (comienzo in comienzos_de_archivo) {
    lista_de_archivos <- list.files(source_raiz, pattern = comienzo)
    ultimo_archivo <- lista_de_archivos[length(lista_de_archivos)]
    cat("Se esta copiando el archivo \n\t", ultimo_archivo, "\nen \n\t", source_raiz, "\n")
    file.copy(ultimo_archivo, carpeta_destino)
  }
  cat("Gracias por utilizar el copiador de archivos\n")
}




#' Evaluanción de rate function (la derivada de la mean)
#'
#' @param t valor a evaluar (real)
#' @param rf_type tipo de rate function (ver articulo A Gibbs Sampling Algorithm
#'   to Estimate the Occurrence of Ozone Exceedances in Mexico City  para el
#'   caso de Weibull (W) y ver libro de Eliane pdf 40 eq (3.12)); posibles
#'   exponentiated-Weibull (EW),the Musa–Okumoto (MO), the Goel–Okumoto (GO),
#'   and a generalized Goel–Okumoto (GGO). Las cuales tienen 3,3,2 y 2
#'   parámetros
#' @param theta parámetros de mean function.Para
#'
#'   W: alpha = theta[1], sigma=theta[2]
#'
#'   EW: alpha=theta[1], beta=theta[2] y sigma=theta[3]
#'
#'   GGO: alpha=theta[1], beta=theta[2] y gamma=theta[3]
#'
#'   MO: alpha=theta[1], beta=theta[2]
#'
#'   GO: alpha=theta[1], beta=theta[2]
#'
#' @return regresa la evaluación
#' @export
#'
#' @examples
#' t <- c(1.4, 2.8)
#' theta <- c(1.2, 2.1, 3.2)
#'
#' rf_type <- "EW"
#' rate_fn(t, rf_type, theta)
#' rf_type <- "GGO"
#' rate_fn(t, rf_type, theta)
#' rf_type <- "MO"
#' rate_fn(t, rf_type, theta)
#' rf_type <- "GO"
#' rate_fn(t, rf_type, theta)
#'
rate_fn <- function(t, rf_type, theta) {
  # W -> theta = c(alpha, sigma)
  if (rf_type == "W") {
    return((theta[1] / theta[2]) * (t / theta[2])^(theta[1] - 1))
  }
  # EW -> theta = c(alpha,beta,sigma)
  if (rf_type == "EW") {
    return(theta[1] * theta[2] * (1 - (exp(-t / theta[3]))^theta[1])^(theta[2] - 1) *
      (exp(-t / theta[3]))^theta[1] *
      (t / theta[3])^(theta[1] - 1) /
      (theta[3] * (1 - (1 - exp(-(t / theta[3])^theta[1]))^theta[2])))
  }
  # GGO -> theta = c(alpha,beta,gamma)
  if (rf_type == "GGO") {
    return(theta[1] * theta[2] * theta[3] * t^(theta[3] - 1) * exp(-theta[2] * t^theta[3]))
  }
  # MO -> theta = c(alpha,beta)
  if (rf_type == "MO") {
    return(theta[2] / (t + theta[1]))
  }
  # GO -> theta = c(alpha,beta)
  if (rf_type == "GO") {
    return(theta[1] * theta[2] * exp(-theta[2] * t))
  } else {
    print("no se tiene esa rf_type; rate_fn")
  }
}



#' Graficando un ajuste de NHPP
#'
#' Esta función solo grafica un ajuste de NHPP
#'
#' @param d_i vector de días de revases
#' @param tau1
#' @param tau2
#' @param mat_phi
#' @param mat_low_upp
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
