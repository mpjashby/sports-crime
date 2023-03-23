# This script creates a dataset of (a) assaults and (b) DUI from Jan 2010 to Dec
# 2019 inclusive, for each city in the analysis.

# Load packages
library(crimedata)
library(tidyverse)

# Due to an issue with the OSF API, we need to download 2010 data separately
# More details: https://github.com/ropensci/osfr/issues/142
download.file(
  url = "https://osf.io/download/mgd3v/",
  destfile = file_2010 <- tempfile(fileext = ".rds")
)
data_2010 <- read_rds(file_2010) |>
  type_convert()

# Get crime data and save to local file
crime_data <- get_crime_data(years = 2010:2019, type = "core") |>
  bind_rows(data_2010) |>
  filter(
    # Filter only cities for which we have sports data
    city_name %in% c(
      "Austin", "Chicago", "Detroit", "Kansas City", "Los Angeles",
      "Louisville", "Memphis", "New York", "Seattle", "St Louis",
      "San Francisco", "Tucson"
    ),
    # See https://osf.io/zyaqn/wiki/2.%20What%20data%20are%20available/ for more
    # details of offence codes
    offense_code %in% c(
      "09A", # Murder and Nonnegligent Manslaughter
      "09B", # Negligent Manslaughter
      "11A", # Rape (except Statutory Rape)
      "11B", # Sodomy
      "11C", # Sexual Assault With An Object
      "11D", # Fondling
      # Some cities don't fully break down sexual assaults, so for those cities
      # the data has a catch-all category:
      "11U", # Sexual Assault by Penetration (including rape)
      "13A", # Aggravated assault
      "13B", # Simple assault
      "90D"  # Driving Under the Influence
    )
  ) |>
  # Reduce the size of the dataset as much as possible
  mutate(
    city = case_match(
      city_name,
      "Austin" ~ "aus",
      "Chicago" ~ "chi",
      "Detroit" ~ "dtt",
      "Kansas City" ~ "kcm",
      "Los Angeles" ~ "lax",
      "Louisville" ~ "lou",
      "Memphis" ~ "mem",
      "New York" ~ "nyc",
      "San Francisco" ~ "sfo",
      "Seattle" ~ "sea",
      "St Louis" ~ "stl",
      "Tucson" ~ "tus"
    ),
    date_time = format(date_single, format = "%Y-%m-%d %H:%M"),
  ) |>
  select(city, offense_code, date_time, longitude, latitude) |>
  arrange(date_time, city, offense_code) |>
  write_csv(here::here("analysis_data/crime_data.csv.gz")) |>
  glimpse()
