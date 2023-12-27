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
#'   GABoltzmannMutation2(DataCPSimRebases, param)
#'   GABoltzmannMutation2(sims$ts1, param)
#'   
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
}
