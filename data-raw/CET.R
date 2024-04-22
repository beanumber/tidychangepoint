library(tidyverse)

## Central England temperature

CET <- read_csv(here::here("data-raw", "CentralEnglandT1659-2020_yearly.csv")) |>
  rename(mean_temp = avg) |>
  mutate(year = parse_date(as.character(year), format = "%Y")) |>
  xts::as.xts()

usethis::use_data(CET, overwrite = TRUE, compress = "xz")
