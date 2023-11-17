library(tidyverse)

pm_25 <- read_csv("data-raw/PM2.5.csv")

usethis::use_data(pm_25, overwrite = TRUE)
