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
  xts::as.xts()

usethis::use_data(bogota_pm, overwrite = TRUE, compress = "xz")


## Medellin rainfall

medellin_rainfall <- readxl::read_excel(here::here("data-raw", "precipitacioìn_mensual.xlsx")) |>
  mutate(
    date = ymd(fecha), 
    precipitation = ifelse(spi_1 == -Inf, NA, as.double(spi_1))
  ) |>
  select(date, precipitation) |>
  xts::as.xts()

usethis::use_data(medellin_rainfall, overwrite = TRUE, compress = "xz")

## Central England temperature

CET <- read_csv(here::here("data-raw", "CentralEnglandT1659-2020_yearly.csv")) |>
  rename(mean_temp = avg) |>
  mutate(year = parse_date(as.character(year), format = "%Y")) |>
  xts::as.xts()

usethis::use_data(CET, overwrite = TRUE, compress = "xz")
