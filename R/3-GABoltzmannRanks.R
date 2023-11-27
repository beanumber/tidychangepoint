#' @rdname MaMBOltzmann
#' @description
#' What does this function do? 
#' 
#' @export
#' @param x An `integer` vector. See [DataCPSimRebases].
#' @param param A `list` or parameters. See [param].
#' @return A `list` of length 2:
#'   - A `numeric` vector of `FitnessGeneraciones`
#'   - A `matrix` with `k` rows and `N` columns
#' @examples
#' \dontrun{
#'   GABoltzmannMutation2(param)
#' }

GABoltzmannMutation2 <- function(x, param, Mutation = 0.03, Temperature = 27) {
  N <- max(x)

  mat_cp <- matrix(NA, nrow = param$k, ncol = N)

  mat_cp <- t(replicate(param$k, RandomKeys(N)))

  FitnessGen <- numeric(0)

  BestChromosomes <- matrix(NA, nrow = 1, ncol = ncol(mat_cp))

  for (i in 1:param$r) {
    Fitness <- Bayesaian_MDL_k_cp(
      mat_cp, x, param$rf_type, param$initial_val_optim,
      param$mat_low_upp, param$vec_dist_a_priori, param$mat_phi,
      param$ajuste_bloque
    )

    FitnessGen[i] <- min(Fitness)

    BestChromosomes <- rbind(
      BestChromosomes,
      mat_cp[which(Fitness == FitnessGen[i]), ]
    )

    mat_cp <- MaMBOltzmann(mat_cp, Fitness, Mutation, Temperature)
  }

  ListResults <- list(
    FitnessGeneraciones = FitnessGen,
    MejoresCromosomas = BestChromosomes[-1, ]
  )

  ListResults
  invisible(list2env(ListResults, envir = .GlobalEnv))
  
  # Plots -----
  
  where_minimo_BMDL <- which.min(FitnessGeneraciones)
  cromosoma_minimo_BMDL <- MejoresCromosomas[where_minimo_BMDL,]
  
  
  par(mfrow=c(2,2), mar = c(3.3, 3.5, 1.5, 1.5), mgp=c(2.2,1,0),
      cex.main = 0.75, cex.lab = 0.7, cex.axis = 0.7)
  
  
  ## Plot a) -----
  
  d <- x
  mejor_cp <- MejoresCromosomas[which.min(FitnessGeneraciones),]
  n_puntos_cambio<-mejor_cp[1]
  tau <- mejor_cp[3:(n_puntos_cambio+2)]
  cromo <- mejor_cp[1:(n_puntos_cambio+3)]
  mat_MAP <- extrae_mat_MAP(cromo,x,param$rf_type,param$initial_val_optim,
                            param$mat_low_upp,param$vec_dist_a_priori,param$mat_phi) 
  sigma<-mat_MAP[,3]
  alpha<-mat_MAP[,2]
  IntensityParam <- list(d = d, tau = tau, alpha = alpha, sigma = sigma)
  invisible(list2env(IntensityParam, envir = .GlobalEnv))
  
  source("funcion_media_acumulada.R")
  grafica_escalonada(x, "black")
  tasa_NHPP <- funcion_media_acumulada()
  upp_bond <- qpois(.95,lambda = c(pow(10/sigma[1],alpha[1]), tasa_NHPP))
  low_bond <- qpois(.05,lambda = c(pow(10/sigma[1],alpha[1]), tasa_NHPP))
  mean_NHPP <-  c(pow(10/sigma[1],alpha[1]), tasa_NHPP)
  lines(c(10, d),upp_bond,col="blue", lwd=2)
  lines(c(10, d),mean_NHPP,col="red", lwd=2)
  lines(c(10, d),low_bond,col="blue", lwd=2)
  
  
  # Plot b) -----
  
  plot(FitnessGeneraciones,
       xlim = c(1,param$r),type="l",col="blue",ylab="BMDL",xlab="Generations",main=paste0("(b) AG with rate ",param$rf_type,"\nand priors ",paste(param$vec_dist_a_priori,collapse = "-")))
  
  # Plot c) -----
  
  plot(table(MejoresCromosomas[,-1][MejoresCromosomas[,-1]>1 & MejoresCromosomas[,-1] < N])/param$r * 100,
       col = 'grey3', 
       main= paste0("(c) Frequency of t in \n", param$r, " generations"),
       xlab = 'Time units - t',  ylab = 'Frequency of t',
       xlim = range(0, x))
  
  # Plot d) -----
  
  plot(MejoresCromosomas[,1],
       xlab = 'Generations', ylab="Number of change-points",
       type="l",col="blue",
       main=paste0("(d) Optimal chromosome: \n has ",MejoresCromosomas[which.min(FitnessGeneraciones), 1]," change-points"))
  abline(h = cromosoma_minimo_BMDL[1], lty = 2)
  abline(v = where_minimo_BMDL, lty = 2)
  par(mfrow=c(1,1))
  
}
