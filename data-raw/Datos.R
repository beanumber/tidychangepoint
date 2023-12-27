library(tidyverse)
set.seed(21)

# simulated data set 1
ts_1 <- c(
  rlnorm(825, 3.5, sdlog = 0.3162278),
  rlnorm(271, 4, sdlog = 0.3162278)
) |>
  as.ts()

# simulated data set 2
ts_2 <- c(
  rlnorm(365, 3.5, sdlog = 0.3162278),
  rlnorm(365, 4, sdlog = 0.3162278),
  rlnorm(366, 4.5, sdlog = 0.3162278)
) |>
  as.ts()

# simulated data set 3
ts_3 <- c(
  rlnorm(547, 3.5, sdlog = 0.3162278),
  rlnorm(275, 4, sdlog = 0.3162278),
  rlnorm(150, 4.5, sdlog = 0.3162278),
  rlnorm(124, 5, sdlog = 0.3162278)
) |>
  as.ts()

sims <- tibble(
  ts1 = ts_1,
  ts2 = ts_2,
  ts3 = ts_3
)

usethis::use_data(sims, overwrite = TRUE)
