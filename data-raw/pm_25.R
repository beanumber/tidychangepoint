library(tidyverse)

## Bogota precipitation

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


## Medellin rainfall

medellin_rainfall <- readxl::read_excel(here::here("data-raw", "precipitacioìn_mensual.xlsx")) |>
  select(date = fecha, monthly_precipitation = spi_1) |>
  mutate(
    monthly_precipitation = as.double(monthly_precipitation),
    monthly_precipitation = ifelse(monthly_precipitation == -Inf, NA, monthly_precipitation)
  )

usethis::use_data(medellin_rainfall, overwrite = TRUE, compress = "xz")

## Central England temperature

england_temperature <- read_csv(here::here("data-raw", "CentralEnglandT1659-2020_yearly.csv")) |>
  rename(annual_mean_temp = avg) |>
  tsibble::as_tsibble(index = year)

usethis::use_data(england_temperature, overwrite = TRUE, compress = "xz")
