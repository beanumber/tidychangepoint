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
