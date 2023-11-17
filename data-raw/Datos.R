DataCPSim <- c(rlnorm(547, 3.5, sdlog = 0.3162278),
               rlnorm(275, 4, sdlog = 0.3162278),
               rlnorm(150, 4.5, sdlog = 0.3162278),
               rlnorm(124, 5, sdlog = 0.3162278))

DataCPSimRebases <- (1:1096)[DataCPSim>mean(DataCPSim)]

usethis::use_data(DataCPSim, overwrite = TRUE)
usethis::use_data(DataCPSimRebases, overwrite = TRUE)
