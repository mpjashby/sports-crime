# This script merges the crime counts and event data to give the final data
# needed for the modelling. The output should be a single CSV file for each of
# the three models (venue, downtown, elsewhere).



# Load packages ----------------------------------------------------------------

library(ggspatial)
library(here)
library(sf)
library(suncalc)
library(tidyverse)



# Load data --------------------------------------------------------------------

# Load crime data
crime_counts <- here("analysis_data/crime_counts.csv.gz") |>
  read_csv() |>
  filter(offence_type == "assault") |>
  # Categorise dates as weekday or weekends
  mutate(weekday = !wday(offence_date, label = TRUE) %in% c("Sat", "Sun")) |>
  rename(crime_count = n)

# Load event data
events <- here("analysis_data") |>
  dir("^events_", full.names = TRUE) |>
  set_names() |>
  map(\(x) read_csv(x, show_col_types = FALSE)) |>
  bind_rows(.id = "sport") |>
  mutate(
    # Extract sport name from file path
    sport = str_remove(
      str_remove(sport, here("analysis_data/events_")),
      fixed(".csv.gz")
    )
  ) |>
  select(city, everything()) |>
  arrange(city, sport, venue, date_time)

# Load city boundaries
city_centroids <- here("original_data/city_outlines.gpkg") |>
  read_sf() |>
  st_centroid() |>
  df_spatial() |>
  select(city = city_name, lon = x, lat = y)



# Get daylight length ----------------------------------------------------------
day_lengths <- city_centroids |>
  # Nest data by city so we can process each city separately, which is necessary
  # because `getSunlightTimes()` needs a single lon/lat pair
  nest(.by = city) |>
  # Calculate (civil) dawn and dusk times for each city on each date
  mutate(
    day_lengths = map(data, function(data) {

      getSunlightTimes(
        date = seq.Date(
          from = min(crime_counts$offence_date),
          to = max(crime_counts$offence_date),
          by = "days"
        ),
        lat = data$lat,
        lon = data$lon,
        keep = c("dawn", "dusk")
      )

    })
  ) |>
  unnest(day_lengths) |>
  # Calculate length of daylight in minutes
  mutate(daylight_mins = as.numeric(difftime(dusk, dawn, units = "mins"))) |>
  select(city, date, daylight_mins)



# Collapse events data to a daily summary --------------------------------------

# The venue models need data in a format with one row per venue per day. This is
# complicated by (a) double headers, which are represented by multiple rows in
# the data, and (b) venues which host events for more than one sport on the same
# day.
events_by_venue <- events |>
  mutate(
    # Extract date of event
    date = as_date(date_time),
    # Match the venue names in the event data (which use the name of the venue
    # on the date of the event) to those used in the stadium buffers data (which
    # use the name at the end of the data period).
    venue = case_match(
      venue,
      "U.S. Cellular Field" ~ "Guaranteed Rate Field",
      "Banc of California Stadium" ~ "BMO Stadium",
      "STAPLES Center" ~ "crypto.com Arena",
      "Edward Jones Dome" ~ "The Dome at America's Center",
      "AT&T Park" ~ "Oracle Park",
      c("CenturyLink Field", "Qwest Field") ~ "Lumen Field",
      "Safeco Field" ~ "T-Mobile Park",
      .default = venue
    ),
    # Allocate the venues to complexes, using the same rules as are used to
    # generate the buffer zone around each complex
    complex = case_match(
      venue,
      # AUSTIN
      "Darrell K Royal-Texas Memorial Stadium" ~
        "Austin (Texas Memorial Stadium)",
      # CHICAGO
      "Guaranteed Rate Field" ~ "Chicago (Comiskey Park)",
      # DETROIT
      c("Comerica Park", "Ford Field", "Little Caesars Arena") ~
        "Detroit (Fisher Drive)",
      # KANSAS CITY
      c("Arrowhead Stadium", "Kauffman Stadium") ~
        "Kansas City (Truman Sports Complex)",
      # LOS ANGELES
      c("BMO Stadium", "Los Angeles Memorial Coliseum") ~
        "Los Angeles (Exposition Park)",
      c("crypto.com Arena", "STAPLES Center") ~ "Los Angeles (Downtown)",
      # SEATTLE
      c("Lumen Field", "T-Mobile Park") ~ "Seattle (SoDo)",
      # ST LOUIS
      c(
        "Busch Stadium",
        "Enterprise Center",
        "Scottrade Center",
        "The Dome at America's Center"
      ) ~
        "St Louis (Downtown)",
      # Venues that are not near any other venues and had a single name
      # throughout the period of the data become complexes on their own
      .default = str_glue("{city} ({venue})")
    )
  ) |>
  # Count number of daytime and night-time events, where daytime events begin
  # at or before 16:59 hours. This deals with double headers.
  summarise(
    event_daytime = any(hour(date_time) <= 16),
    event_nighttime = any(hour(date_time) >= 17),
    .by = c(complex, sport, date)
  ) |>
  # Identify where there are multiple sports being played at the same venue on
  # the same day.
  summarise(
    sport = if_else(n() > 1, "multiple", str_to_upper(first(sport))),
    time = case_when(
      any(event_daytime) == TRUE & any(event_nighttime) == TRUE ~ "both",
      any(event_daytime) == TRUE ~ "day",
      any(event_nighttime) == TRUE ~ "night",
      TRUE ~ NA_character_
    ),
    .by = c(complex, date)
  )

# The downtown and rest-of-city models need data in the same city-by-day format,
# so we can create that format first and then extract the data for each area
# type in the next section of the code
events_by_day <- events |>
  # Extract date of event
  mutate(date = as_date(date_time)) |>
  # Count number of daytime and night-time events, where daytime events begin
  # at or before 16:59 hours
  summarise(
    day = any(hour(date_time) <= 16),
    night = any(hour(date_time) >= 17),
    .by = c(city, sport, date)
  ) |>
  # Make dataset longer so every event has its own row
  pivot_longer(
    cols = where(is.logical),
    names_to = "period",
    values_to = "event_in_period"
  ) |>
  # Remove unnecessary rows representing periods without events
  filter(event_in_period == TRUE) |>
  # Create a single variable representing both the sport and the time of day
  mutate(event = str_glue("{sport}_{period}")) |>
  # Remove unnecessary columns
  select(city, date, event, event_in_period) |>
  # Make dataset wider so each row represents one day in one city
  pivot_wider(
    names_from = event,
    values_from = event_in_period,
    values_fill = FALSE
  )



# Merge datasets ---------------------------------------------------------------


## Venue data ----
crimes_by_venue <- crime_counts |>
  filter(area == "venue") |>
  rename(date = offence_date) |>
  # Join day lengths
  left_join(day_lengths, by = c("city", "date")) |>
  # Join events
  left_join(events_by_venue, by = c("complex", "date")) |>
  # Replace missing values of `sport` and `time` variables, which represent days
  # on which there were no events in a particular complex
  replace_na(list(sport = "none", time = "none")) |>
  # Replace missing values of events variables -- which represent days in which
  # there were no events in a particular sport -- with `FALSE`
  mutate(across(where(is.logical), \(x) if_else(is.na(x), FALSE, x))) |>
  select(
    city,
    complex,
    offence_type,
    date,
    crime_count,
    weekday,
    daylight_mins,
    sport,
    time
  ) |>
  write_csv(here("analysis_data/model_data_venue.csv.gz")) |>
  glimpse()


## Downtown data ----
crime_counts |>
  filter(area == "ent") |>
  rename(date = offence_date) |>
  select(-complex) |>
  # Join day lengths
  left_join(day_lengths, by = c("city", "date")) |>
  # Join events
  left_join(events_by_day, by = c("city", "date")) |>
  # Replace missing values of events variables -- which represent days in which
  # there were no events in a particular sport -- with `FALSE`
  mutate(across(where(is.logical), \(x) if_else(is.na(x), FALSE, x))) |>
  select(-area) |>
  write_csv(here("analysis_data/model_data_downtown.csv.gz")) |>
  glimpse()


## Rest-of-city data ----
crime_counts |>
  filter(area == "rest") |>
  rename(date = offence_date) |>
  select(-complex) |>
  # Join day lengths
  left_join(day_lengths, by = c("city", "date")) |>
  # Join events
  left_join(events_by_day, by = c("city", "date")) |>
  # Replace missing values of events variables -- which represent days in which
  # there were no events in a particular sport -- with `FALSE`
  mutate(across(where(is.logical), \(x) if_else(is.na(x), FALSE, x))) |>
  select(-area) |>
  write_csv(here("analysis_data/model_data_rest_of_city.csv.gz")) |>
  glimpse()
