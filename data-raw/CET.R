library(tidyverse)

## Central England temperature

CET20 <- read_csv(here::here("data-raw", "CentralEnglandT1659-2020_yearly.csv")) |>
  rename(mean_temp = avg) |>
  mutate(year = parse_date(as.character(year), format = "%Y")) |>
  xts::as.xts()

# usethis::use_data(CET20, overwrite = TRUE, compress = "xz")

CET <- read_table("https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_monthly_totals.txt", skip = 4) |>
  mutate(year = parse_date(as.character(Year), format = "%Y")) |>
  select(year, mean_temp = Annual) |>
  xts::as.xts()

usethis::use_data(CET, overwrite = TRUE, compress = "xz")
