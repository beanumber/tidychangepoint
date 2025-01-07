library(tidyverse)
library(Lahman)

mlb_lg <- Batting |>
  filter(yearID > 1924) |>
  group_by(yearID, lgID) |>
  summarize(
    tAB = sum(AB),
    tH = sum(H),
    tBB = sum(BB, na.rm = TRUE),
    tHBP = sum(HBP, na.rm = TRUE),
    tSF = sum(SF, na.rm = TRUE),
    tSH = sum(SH, na.rm = TRUE),
    tHR = sum(HR),
    tTB = sum(H + X2B + 2*X3B + 3*HR)
  ) |>
  group_by(yearID, lgID) |>
  mutate(
    PA = sum(tAB + tBB + tHBP + tSF + tSH),
    hr_rate = sum(tHR) / sum(PA),
    bavg = sum(tH) / sum(tAB),
    obp = sum(tH + tBB + tHBP) / sum(PA - tSH),
    slg = sum(tTB) / sum(tAB)
  ) |>
  group_by(yearID) |>
  arrange(yearID, lgID) 

mlb_diffs <- mlb_lg |>
  summarize(
    PA = sum(PA),
    hr_rate_diff = diff(hr_rate),
    bavg_diff = diff(bavg),
    obp_diff = diff(obp),
    slg_diff = diff(slg)
  ) |>
  mutate(yearID = parse_date(as.character(yearID), format = "%Y")) |>
  tsibble::as_tsibble()

ggplot(mlb_lg, aes(x = yearID, y = bavg, color = lgID)) +
  geom_line()

ggplot(mlb_diffs, aes(x = yearID, y = bavg_diff)) +
  geom_line() +
  geom_vline(xintercept = c(1973, 2022), linetype = 3)

usethis::use_data(mlb_diffs, overwrite = TRUE, compress = "xz")
