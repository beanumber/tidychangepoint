library(tidyverse)

## Medellin rainfall

mde_rain <- read_csv(here::here("data-raw/medellin.csv"))
usethis::use_data(mde_rain, overwrite = TRUE, compress = "xz")

mde_rain_monthly <- mde_rain |>
  group_by(year, month) |>
  summarize(
    mean_rainfall = mean(rainfall, na.rm = TRUE)
  ) |>
  mutate(date = ym(paste(year, month))) |>
  ungroup() |>
  select(date, mean_rainfall) |>
  xts::as.xts()
usethis::use_data(mde_rain_monthly, overwrite = TRUE, compress = "xz")





### OLD STUFF

medellin_rainfall <- readxl::read_excel(here::here("data-raw", "precipitacioìn_mensual.xlsx")) |>
  mutate(
    date = ymd(fecha), 
    precipitation = ifelse(spi_1 == -Inf, NA, as.double(spi_1))
  ) |>
  select(date, precipitation) |>
  xts::as.xts()

usethis::use_data(medellin_rainfall, overwrite = TRUE, compress = "xz")

## Medellin rainfall

medellin_rainfall2 <- readxl::read_excel(here::here("data-raw", "Datos_spi_estacion1.xlsx")) |>
  mutate(
    date = parse_date(paste(year, month), "%Y %m")
  ) |>
  select(date, rainfall, spi) |>
  xts::as.xts()
