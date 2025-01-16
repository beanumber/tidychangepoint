library(tidyverse)

## Italian graduates

coverage <- tibble::tibble(
  sheet = 1:4,
  range = rep(c("A6:I62", "A6:I40"), 2)
)  

xl <- coverage |>
  purrr::pmap(readxl::read_excel, path = here::here("data-raw/Table_7.16.xls"), na = "-")

italy_grads <- bind_cols(
  bind_rows(xl[[1]], xl[[2]]),
  bind_rows(xl[[3]], xl[[4]])
) |>
  janitor::clean_names() |>
  filter(!is.na(academic_years_1)) |>
  rename(
    year = academic_years_1,
    total = total_v_a_100_0
  ) |>
  mutate(
    total = if_else(is.na(total), total_d_v_a_100_0, total)
  ) |>
  select(-academic_years_10, -total_d_v_a_100_0) |>
  mutate(year = parse_date(as.character(year), format = "%Y")) |>
  tsibble::tsibble()

usethis::use_data(italy_grads, overwrite = TRUE, compress = "xz")
