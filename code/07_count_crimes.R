# This code counts the daily number of violent crimes occurring in each area of
# each city

# Load packages ----------------------------------------------------------------

library(here)
library(sf)
library(tsibble)
library(tidyverse)



# Load data --------------------------------------------------------------------

## Load crime data ----

crimes <- here("analysis_data/crime_data.csv.gz") |>
  read_csv() |>
  mutate(
    city = case_match(
      city,
      "aus" ~ "Austin",
      "chi" ~ "Chicago",
      "dtt" ~ "Detroit",
      "kcm" ~ "Kansas City",
      "lax" ~ "Los Angeles",
      "nyc" ~ "New York",
      "sfo" ~ "San Francisco",
      "sea" ~ "Seattle",
      "stl" ~ "St Louis",
      "tus" ~ "Tucson"
    ),
    # If a crime happened before 05:00 hours, record it as occurring on the
    # previous day to create 'days' that run from 05:00 to 04:59 hours
    offence_date = if_else(
      hour(date_time) < 5,
      as_date(date_time) - days(1),
      as_date(date_time)
    ),
    # Categorise offence types
    offence_type = case_match(
      offense_code,
      c("09A", "09B", "11A", "11B", "11C", "11D", "11U", "13A", "13B") ~
        "assault",
      "90D" ~ "DUI"
    )
  ) |>
  # A few crimes will now be recorded as occurring outside the time period we
  # are analysing, so filter these out
  filter(between(offence_date, ymd("2010-01-01"), ymd("2019-12-31"))) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
  # Keep only columns we need, since the dataset is very large
  select(city, offence_date, offence_type)


## Load city boundaries ----

city_boundaries <- here("original_data/city_outlines.gpkg") |>
  read_sf() |>
  rename(city = city_name)


## Load stadium buffers ----

venues <- read_sf(here("analysis_data/venue_buffers.gpkg"))


## Load entertainment districts ----

ent_districts <- read_sf(here("analysis_data/entertainment_districts.gpkg"))



# Create remaining areas of cities ---------------------------------------------

rest_of_city <- ent_districts |>
  pull("city") |>
  map(function(this_city) {

    # Extract areas for this city
    this_ent <- filter(ent_districts, city == this_city)
    this_venues <- venues |>
      filter(city == this_city) |>
      st_union()

    # Cut out areas from city boundary
    city_boundaries |>
      filter(city == this_city) |>
      st_difference(this_ent) |>
      st_difference(this_venues) |>
      select(city)

  }) |>
  bind_rows()



# Count crimes -----------------------------------------------------------------


## Count crimes around venues ----
counts_venue <- crimes |>
  arrange(city) |>
  nest(.by = city) |>
  mutate(
    counts = map2(
      city,
      data,
      function(this_city, city_data) {

        this_city_venues <- venues |>
          filter(city == this_city) |>
          select(venue)

        city_data |>
          st_join(this_city_venues) |>
          st_drop_geometry() |>
          filter(!is.na(venue)) |>
          count(offence_type, venue, offence_date)

      },
      .progress = TRUE
    )
  ) |>
  select(city, counts) |>
  unnest(counts)


## Count crimes in entertainment districts ----
counts_ent <- crimes |>
  arrange(city) |>
  nest(.by = city) |>
  mutate(
    counts = map2(
      city,
      data,
      function(this_city, city_data) {

        this_city_ent <- ent_districts |>
          filter(city == this_city) |>
          select()

        city_data |>
          st_join(this_city_ent) |>
          st_drop_geometry() |>
          count(offence_type, offence_date)

      },
      .progress = TRUE
    )
  ) |>
  select(city, counts) |>
  unnest(counts)


## Count crimes in the rest of each city ----
counts_rest <- crimes |>
  arrange(city) |>
  nest(.by = city) |>
  mutate(
    counts = map2(
      city,
      data,
      function(this_city, city_data) {

        this_city_rest <- rest_of_city |>
          filter(city == this_city) |>
          select()

        city_data |>
          st_join(this_city_rest) |>
          st_drop_geometry() |>
          count(offence_type, offence_date)

      },
      .progress = TRUE
    )
  ) |>
  select(city, counts) |>
  unnest(counts)


## Combine counts and fill in missing dates ----
bind_rows(
  "venue" = counts_venue,
  "ent" = counts_ent,
  "rest" = counts_rest,
  .id = "area"
) |>
  # Conver to tsibble so we can use `fill_gaps()`, which requires a time-series
  # aware dataset
  as_tsibble(key = c(city, area, offence_type, venue), index = offence_date) |>
  # Fill gaps in the time series that represent dates with zero crimes in a
  # particular area in a particular city
  fill_gaps(
    n = 0,
    .full = TRUE,
    .start = ymd("2010-01-01"),
    .end = ymd("2019-12-31")
  ) |>
  # Move city to be the first column
  select(city, everything()) |>
  write_csv(here("analysis_data/crime_counts.csv.gz")) |>
  glimpse()

