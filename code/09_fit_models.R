# This code runs models for each of the three areas in each city: venues,
# downtowns and rest-of-city. The output from this file should be three model
# objects stored in .rds files.



# PREPARE ----------------------------------------------------------------------


# Load packages ----
library(here)
library(brms)
library(tidybayes)
library(tidyverse)


# Load data ----
data_venue <- here("analysis_data/model_data_venue.csv.gz") |>
  read_csv() |>
  filter(offence_type == "assault") |>
  mutate(crime_count_lag = lag(crime_count), .by = complex) |>
  mutate(
    daylight_mins = as.numeric(scale(daylight_mins)),
    sport = fct_relevel(sport, "none"),
    time = fct_relevel(time, "none")
  )

data_downtown <- here("analysis_data/model_data_downtown.csv.gz") |>
  read_csv() |>
  filter(offence_type == "assault")

data_restofcity <- here("analysis_data/model_data_rest_of_city.csv.gz") |>
  read_csv() |>
  filter(offence_type == "assault")



# VENUE MODELS -----------------------------------------------------------------


## Model for venues on all days/times ----
model_venue_all <- brm(
  formula = crime_count ~ crime_count_lag +
    (1 + daylight_mins + sport | complex),
  data = data_venue,
  # Use a negative binomial model since the dependent variable is a count
  # variable with lots of zeros
  family = negbinomial(),
  prior = set_prior("normal(0,0.1)", class = "b", coef = "crime_count_lag"),
  # Run the chains in parallel using multiple processor cores (there are four
  # chains by default, so using more than four cores would not speed up the
  # model estimation any further)
  cores = 4,
  # Save a copy of the model in a `.rds` file (`brm()` will add the `.rds`
  # file extension)
  file = here("analysis_data/model_venue_all"),
  # Re-estimate the model if the specification has changed relative to the model
  # saved in the above file, otherwise re-load the model from the file
  file_refit = "on_change",
  # Specify starting value for MCMC chains (as recommended in `?mixture`)
  init = 0
)


## Model for venues on weekdays/weekends ----
model_venue_day <- brm(
  formula = crime_count ~ crime_count_lag +
    (1 + daylight_mins + sport * weekday | complex),
  data = data_venue,
  # Use a negative binomial model since the dependent variable is a count
  # variable with lots of zeros
  family = negbinomial(),
  prior = set_prior("normal(0,0.1)", class = "b", coef = "crime_count_lag"),
  # Run the chains in parallel using multiple processor cores (there are four
  # chains by default, so using more than four cores would not speed up the
  # model estimation any further)
  cores = 4,
  # Save a copy of the model in a `.rds` file (`brm()` will add the `.rds`
  # file extension)
  file = here("analysis_data/model_venue_day"),
  # Re-estimate the model if the specification has changed relative to the model
  # saved in the above file, otherwise re-load the model from the file
  file_refit = "on_change",
  # Specify starting value for MCMC chains (as recommended in `?mixture`)
  init = 0
)


## Model for venues on weekdays/weekends in daytime/evening ----
model_venue_day_time <- brm(
  formula = crime_count ~ crime_count_lag +
    (1 + daylight_mins + sport * weekday * time | complex),
  data = data_venue,
  # Use a negative binomial model since the dependent variable is a count
  # variable with lots of zeros
  family = negbinomial(),
  prior = set_prior("normal(0,0.1)", class = "b", coef = "crime_count_lag"),
  # Run the chains in parallel using multiple processor cores (there are four
  # chains by default, so using more than four cores would not speed up the
  # model estimation any further)
  cores = 4,
  # Save a copy of the model in a `.rds` file (`brm()` will add the `.rds`
  # file extension)
  file = here("analysis_data/model_venue_day_time"),
  # Re-estimate the model if the specification has changed relative to the model
  # saved in the above file, otherwise re-load the model from the file
  file_refit = "on_change",
  # Specify starting value for MCMC chains (as recommended in `?mixture`)
  init = 0
)


# For advice on interpreting diagnostic plots for this model, see
# https://m-clark.github.io/posts/2021-02-28-practical-bayes-part-i/
