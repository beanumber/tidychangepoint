library(tidyverse)
library(Lahman)

mlb_hrs <- Batting |>
  filter(yearID > 1924) |>
  group_by(yearID, lgID) |>
  summarize(
    tAB = sum(AB),
    tBB = sum(BB, na.rm = TRUE),
    tHBP = sum(HBP, na.rm = TRUE),
    tSF = sum(SF, na.rm = TRUE),
    tSH = sum(SH, na.rm = TRUE),
    tHR = sum(HR)
  ) |>
  group_by(yearID, lgID) |>
  mutate(
    PA = sum(tAB + tBB + tHBP + tSF + tSH),
    hr_rate = sum(tHR) / sum(PA)
  ) |>
  group_by(yearID) |>
  arrange(yearID, lgID) |>
  summarize(hr_rate_diff = diff(hr_rate)) |>
  pull(hr_rate_diff) |>
  ts(start = 1925, frequency = 1) |>
  xts::as.xts()
  
usethis::use_data(mlb_hrs, overwrite = TRUE, compress = "xz")
