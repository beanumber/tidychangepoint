

param <- list(
  r = 50, # número de generaciones 1000
  k = 50,  # tamaño de generaciones 200
  max_num_cp = 20,
  prob_inicial = 0.06, # = 0.05 bueno, = 0.2
  probs_muta = c(.3,.4,.3), # c(.3,.4,.3)
  mutaciones = c(-1,0,1),
  dist_extremos = 10,
  prob_para_sin_cp = 0.5,
  cuantos_mejores_cp_graf = 100, # cuantos de los mejores cp se graficarán
  minimo_numero_de_cp = 1, # minimo número de puntos de cambio
  rf_type = c("W","EW","GGO","MO","GO")[1], # función de tasa de NHPP
  vec_dist_a_priori = c("Gamma","Gamma"), # distribuciones a priori
  mat_phi = matrix(c(1,3,2,1.2),ncol = 2)
) # parametros de dist a priori, cada renglon corresponde a una dist parametro

usethis::use_data(param, overwrite = TRUE)
