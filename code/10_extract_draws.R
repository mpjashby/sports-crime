# This code takes the model objects produced in 09_fit_models.R and extracts
# the draws from each object. The draws can then be used to analyse the
# distribution of estimates for each term in each model.



# PREPARE ----------------------------------------------------------------------


# Load packages ----
library(here)
library(tidybayes)
library(tidyverse)



# VENUE MODELS -----------------------------------------------------------------


## Model for venues on all days/times ----
here("analysis_data/model_venue_all.rds") |>
  read_rds() |>
  spread_draws(r_complex[complex,term]) |>
  filter(str_detect(term, "^sport")) |>
  median_qi() |>
  mutate(
    across(c(r_complex, .lower, .upper), exp),
    sig = !(.lower <= 1 & .upper >= 1),
    sport = str_remove(term, "sport"),
    complex = str_squish(str_replace_all(complex, fixed("."), " ")),
    stadium = str_glue("{complex} -- {sport}")
  ) |>
  write_csv(here("analysis_data/draws_venue_all.csv.gz"))


## Model for venues on weekdays/weekends ----
here("analysis_data/model_venue_day.rds") |>
  read_rds() |>
  spread_draws(r_complex[complex,term]) |>
  filter(str_detect(term, "^sport")) |>
  median_qi() |>
  separate(term, into = c("sport", "weekday"), sep = ":", fill = "right") |>
  replace_na(list(weekday = "weekend")) |>
  mutate(
    across(c(r_complex, .lower, .upper), exp),
    sig = !(.lower <= 1 & .upper >= 1),
    sport = str_remove(sport, "sport"),
    complex = str_squish(str_replace_all(complex, fixed("."), " ")),
    stadium = str_glue("{complex} -- {sport}"),
    weekday = str_remove(weekday, "TRUE$")
  ) |>
  write_csv(here("analysis_data/draws_venue_day.csv.gz"))


## Model for venues on weekdays/weekends in daytime/evening ----
