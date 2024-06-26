library(tidyverse)
set.seed(21)

# simulated data set 1
rlnorm_ts_1 <- c(
  rlnorm(825, 3.5, sdlog = 0.3162278),
  rlnorm(271, 4, sdlog = 0.3162278)
) |>
  as.ts()

attr(rlnorm_ts_1, "cpt_true") <- 826

usethis::use_data(rlnorm_ts_1, overwrite = TRUE)

# simulated data set 2
rlnorm_ts_2 <- c(
  rlnorm(365, 3.5, sdlog = 0.3162278),
  rlnorm(365, 4, sdlog = 0.3162278),
  rlnorm(366, 4.5, sdlog = 0.3162278)
) |>
  as.ts()

attr(rlnorm_ts_2, "cpt_true") <- c(366, 731)

usethis::use_data(rlnorm_ts_2, overwrite = TRUE)

# simulated data set 3
rlnorm_ts_3 <- c(
  rlnorm(547, 3.5, sdlog = 0.3162278),
  rlnorm(275, 4, sdlog = 0.3162278),
  rlnorm(150, 4.5, sdlog = 0.3162278),
  rlnorm(124, 5, sdlog = 0.3162278)
) |>
  as.ts()

attr(rlnorm_ts_3, "cpt_true") <- c(548, 823, 973)

usethis::use_data(rlnorm_ts_3, overwrite = TRUE)

## Genetic algorithm

# lista_AG <- segment(DataCPSim, method = "gbmdl")
# usethis::use_data(lista_AG, overwrite = TRUE)

