library(tidyverse)

bogota_pm <- read_csv("data-raw/PM2.5.csv") |>
  mutate(
    year = rep(2018:2020, c(365, 365, 366)),
    yday = c(rep(1:365, 3), 366),
    date = as.Date(parse_date_time(paste(year, yday), "%Y %j")),
    particulate_matter = PM2.5 / 10
  ) |>
  select(date, particulate_matter) |>
  tsibble::as_tsibble(index = date)


usethis::use_data(bogota_pm, overwrite = TRUE, compress = "xz")
