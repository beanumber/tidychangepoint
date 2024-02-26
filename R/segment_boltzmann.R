globalVariables("Bayesaian_MDL_k_cp")

#' Segment a times series using Boltzmann criteria for a genetic algorithm
#' @description
#' What does this function do? 
#' 
#' @export
#' @param x An `integer` vector. See [exceedances()].
#' @param num_generations number of generations
#' @param generation_size size of generation
#' @param mutation_rate description
#' @param temperature description
#' @return A `list` of length 2:
#'   - A `numeric` vector of `FitnessGeneraciones`
#'   - A `matrix` with `k` rows and `N` columns
#' @examples
#' \dontrun{
#' x <- segment(DataCPSim, method = "boltzmann", num_generations = 5)
#' y <- segment_boltzmann(rlnorm_ts_1)
#'   
#' }

segment_boltzmann <- function(x, 
                              num_generations = 10, 
                              generation_size = 50, 
                              mutation_rate = 0.03, 
                              temperature = 27,
                              show_progress_bar = TRUE) {
  obj <- new_seg_default(x, algorithm = "boltzmann", params = list(
    num_generations = num_generations,
    generation_size = generation_size,
    mutation_rate = mutation_rate,
    temperature = temperature
  ))
  
  N <- max(exceedances(x))

  mat_cp <- matrix(NA, nrow = obj$params$num_generations, ncol = N)
  mat_cp <- t(replicate(obj$params$generation_size, RandomKeys(N)))

  best_cpts <- list()
  pb <- utils::txtProgressBar(min = 1, max = obj$params$num_generations, style = 3, width = 60)

  for (i in 1:obj$params$num_generations) {
    if (show_progress_bar) {
      utils::setTxtProgressBar(pb, i)
    }
    this_generation <- mat_cp |>
      mat_cp_2_list() |>
      evaluate_cpts(.data = as.ts(x))
    
    best_cpts[[i]] <- this_generation |>
      dplyr::arrange(bmdl) |>
      utils::head(1)

    mat_cp <- mat_cp |>
      evolve_boltzmann(
        this_generation$bmdl, 
        obj$params$mutation_rate, 
        obj$params$temperature
      )
  }

  obj$candidates <- best_cpts |>
    dplyr::bind_rows()
  
  return(obj)
}


#' @rdname segment_boltzmann
#' @param mat_cp description
#' @param Fitness description
#' @export

evolve_boltzmann <- function(mat_cp, Fitness, Mutation, Temperature) {
  N <- ncol(mat_cp)
  FitnessMatrix <- data.frame(
    Index = rank(-Fitness),
    MDLScore = sort(Fitness,
                    decreasing = TRUE
    )
  )
  k <- dim(mat_cp)[1]
  Offspring <- matrix(NA, nrow = 1, ncol = N)
  
  Rank <- (1:(dim(mat_cp)[1])) / sum(1:(dim(mat_cp)[1]))
  
  
  while (dim(Offspring)[1] < (k + 1)) {
    FatherCandidate1 <- length(Rank[Rank <= stats::runif(1, min(Rank), max(Rank))])
    MotherCandidate1 <- length(Rank[Rank <= stats::runif(1, min(Rank), max(Rank))])
    
    
    if (FatherCandidate1 == 1) {
      FatherCandidate2 <- 2
    } else if (FatherCandidate1 == max(FitnessMatrix$Index)) {
      FatherCandidate2 <- (FatherCandidate1 - 1)
    } else if ((FatherCandidate1 - 2 < 1) |
               ((FatherCandidate1 + 2) > dim(FitnessMatrix)[1])) {
      FatherCandidate2 <- sample(c(
        FatherCandidate1 - 1,
        FatherCandidate1 + 1
      ), 1)
    } else {
      FatherCandidate2 <- sample(c(
        FatherCandidate1 - 2,
        FatherCandidate1 - 1,
        FatherCandidate1 + 1,
        FatherCandidate1 + 2
      ), 1)
    }
    
    # Madre
    
    if (MotherCandidate1 == 1) {
      MotherCandidate2 <- 2
    } else if (MotherCandidate1 == max(FitnessMatrix$Index)) {
      MotherCandidate2 <- (MotherCandidate1 - 1)
    } else if ((MotherCandidate1 - 2 < 1) |
               ((MotherCandidate1 + 2) > dim(FitnessMatrix)[1])) {
      MotherCandidate2 <- sample(c(
        MotherCandidate1 - 1,
        MotherCandidate1 + 1
      ), 1)
    } else {
      MotherCandidate2 <- sample(c(
        MotherCandidate1 - 2,
        MotherCandidate1 - 1,
        MotherCandidate1 + 1,
        MotherCandidate1 + 2
      ), 1)
    }
    
    
    FCBoltzmann1 <- exp(-FitnessMatrix[FatherCandidate1, "MDLScore"] / Temperature) / sum(exp(-FitnessMatrix$MDLScore / Temperature))
    FCBoltzmann2 <- exp(-FitnessMatrix[FatherCandidate2, "MDLScore"] / Temperature) / sum(exp(-FitnessMatrix$MDLScore / Temperature))
    
    MCBoltzmann1 <- exp(-FitnessMatrix[MotherCandidate1, "MDLScore"] / Temperature) / sum(exp(-FitnessMatrix$MDLScore / Temperature))
    MCBoltzmann2 <- exp(-FitnessMatrix[MotherCandidate2, "MDLScore"] / Temperature) / sum(exp(-FitnessMatrix$MDLScore / Temperature))
    
    
    if (FCBoltzmann2 < FCBoltzmann1) {
      FatherIndex <- FatherCandidate2
    } else {
      FatherIndex <- FatherCandidate1
    }
    
    if (MCBoltzmann2 < MCBoltzmann1) {
      MotherIndex <- MotherCandidate2
    } else {
      MotherIndex <- MotherCandidate1
    }
    
    
    FatherIndex <- FitnessMatrix[FatherIndex, "Index"]
    MotherIndex <- FitnessMatrix[MotherIndex, "Index"]
    
    if (FatherIndex != MotherIndex) {
      Child <- c(
        mat_cp[FatherIndex, 3:(mat_cp[FatherIndex, 1] + 2)],
        mat_cp[MotherIndex, 3:(mat_cp[MotherIndex, 1] + 2)]
      )
      
      Child <- unique(sort(Child))
      Child <- Child[stats::runif(length(Child)) <= 0.5]
      
      MutationRate <- stats::runif(1)
      
      if (MutationRate <= Mutation) {
        Child <- Child + sample(c(-1, 0, 1), size = length(Child), prob = c(0.3, 0.4, 0.3), replace = TRUE)
      }
      
      Child <- unique(sort(Child))
      
      Child <- Child[(Child != 1) & (Child != N)]
      
      r <- length(Child)
      
      Child <- c(1, Child, N)
      
      Child <- c(r, Child)
      
      Child <- c(
        Child,
        rep(0, N - length(Child))
      )
      
      CheckChild <- apply(Offspring, 1, identical, Child)
      
      if ((sum(CheckChild) == 0) & (r != 0)) {
        Offspring <- rbind(Offspring, Child)
      }
    }
  }
  
  Offspring <- Offspring[-1, ]
  rownames(Offspring) <- NULL
  
  Offspring
}

#' Generate random keys
#' @param N number of keys
#' @export
#' @examples
#' RandomKeys(5)
#' RandomKeys(24)
#'
RandomKeys <- function(N) {
  RandomKeys <- stats::runif(n = N - 1)
  RandomKeysOrder <- sort(RandomKeys[2:length(RandomKeys)])
  
  R0 <- floor(RandomKeys[1] * (N - 2))
  
  if (R0 == 0) {
    Chromosome <- c(1, N)
  } else {
    Chromosome <- (2:(N - 1))[(RandomKeys[2:length(RandomKeys)]) %in% (RandomKeysOrder[1:R0])]
    Chromosome <- c(1, Chromosome, N)
    Chromosome <- c(R0, Chromosome)
    Chromosome <- c(Chromosome, rep(0, N - length(Chromosome)))
  }
  
  Chromosome
}
