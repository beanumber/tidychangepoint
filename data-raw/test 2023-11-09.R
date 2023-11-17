

param <- list(nombre_datos = c("ciclones","O3","O3_U61","PM10","PM10_U100", "DataCPSimRebases")[6],
              frecuencia_datos = 7, # e.g., 7 equivale a semanal
              n_datos = c("TODOS",403,1003)[1], # tomamos todos o un subconjuto de datos
              diarios0_rebases1 = 0,
              valor_de_rebase = mean(DataCPSim),
              r = 50, # número de generaciones 1000
              k = 50,  # tamaño de generaciones 200
              penalty = c("MDL","BMDL")[2],
              max_num_cp = 20,
              prob_inicial = 0.06, # = 0.05 bueno, = 0.2
              # dist = c("log_norm","poisson","DIC log_norm","DIC poisson")[1],  # cambiar para [2]
              prob_volado = 0.5, # = .5
              probs_muta = c(.3,.4,.3), # c(.3,.4,.3)
              mutaciones = c(-1,0,1),
              dist_extremos = 10,
              prob_para_sin_cp = 0.5,
              cp_real = "sin cp_real",
              quita_ini0_fin1 = 0, # 
              probs_rank0_MDL1 = 0,
              nombre_carpeta_pdf = "Figures",
              nombre_carpeta_RData = "Data",
              cuantos_mejores_cp_graf = 100, # cuantos de los mejores cp se graficarán
              my_data = list(nulo=NULL, my_data=123)[[1]],
              minimo_numero_de_cp = 1, # minimo número de puntos de cambio
              probs_nuevos_muta0N = c(.8,.1,.1), # probabilidades de mutar 0,1,2,... hasta cierto numero
              rf_type = c("W","EW","GGO","MO","GO")[1], # función de tasa de NHPP
              initial_val_optim = c(.1,.5), # valores iniciales de busqueda del MAP
              mat_low_upp = matrix(c(c(1e-4,1e-8),c(1e+1,1e+5)),nrow = 2), # rango de busqueda de MAP
              vec_dist_a_priori = c("Gamma","Gamma"), # distribuciones a priori
              mat_phi = matrix(c(1,3,2,1.2),ncol = 2),
              ajuste_bloque = T,
              print_progress_bar =T,
              print_progress_plots = T,
              value_set_seed = 123) # parametros de dist a priori, cada renglon corresponde a una dist parametro

usethis::use_data(param, overwrite = TRUE)
