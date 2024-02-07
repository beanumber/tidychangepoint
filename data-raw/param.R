

param <- list(
  rf_type = c("W","EW","GGO","MO","GO")[1], # función de tasa de NHPP
  vec_dist_a_priori = c("Gamma","Gamma"), # distribuciones a priori
  mat_phi = matrix(c(1,3,2,1.2),ncol = 2)
) # parametros de dist a priori, cada renglon corresponde a una dist parametro

usethis::use_data(param, overwrite = TRUE)
