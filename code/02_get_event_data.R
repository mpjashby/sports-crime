# This code downloads data on sporting events in different cities and saves
# it in a single file for further analysis. The data for each sport should be
# saved in a separate `.csv.gz` file with the following columns:
#
#   * `city`: city name (chr)
#   * `team`: team name (chr)
#   * `date_time`: date-time at which the event started (dttm)
#   * `result`: whether the game was a win (`W`), loss (`L`) or tie (`T`) (chr)
#   * `attendance`: number of spectators (dbl)



# SETUP ------------------------------------------------------------------------

# Note that the bigballR package is not on CRAN, but is available via
# remotes::install_github("jflancer/bigballR")

# Load sports API packages
library(baseballr)
library(bigballR)
library(rMLS)
library(nbastatR)
library(nflfastR)
library(nhlapi)

# Load data-wrangling packages
library(assertr)
library(rvest)
library(here)
library(janitor)
library(tidyverse)

# Create vector of cities we can later use to check against
cities <- c(
  "Austin", "Chicago", "Detroit", "Kansas City", "Los Angeles", "Louisville",
  "Memphis", "New York", "Seattle", "St Louis", "San Francisco", "Tucson"
)



# MAJOR LEAGUE BASEBALL --------------------------------------------------------

2010:2019 |>
  map(mlb_schedule) |>
  bind_rows() |>
  filter(
    # Remove games that were cancelled, etc.
    status_detailed_state %in% c("Completed Early", "Final"),
    # Keep only games at stadia in the cities of interest
    venue_name %in% c(
      "AT&T Park", # SF Giants, until 2018
      "Busch Stadium", # St Louis Cardinals
      "Citi Field", # NY Mets
      "Comerica Park", # Detroit Tigers
      "Dodger Stadium", # LA Dodgers
      "Guaranteed Rate Field", # Chicago White Sox, from 2017
      "Kauffman Stadium", # Kansas City Royals
      "Oracle Park", # SF Giants, from 2019
      "Safeco Field", # Seattle Mariners, until 2018
      "T-Mobile Park", # Seattle Mariners, from 2019
      "Wrigley Field", # Chicago Cubs
      "U.S. Cellular Field", # Chicago White Sox, until 2016
      "Yankee Stadium" # NY Yankees
    )
  ) |>
  mutate(
    # Get city from venue
    city = case_match(
      venue_name,
      "AT&T Park" ~ "San Francisco",
      "Busch Stadium" ~ "St Louis",
      "Citi Field" ~ "New York",
      "Comerica Park" ~ "Detroit",
      "Dodger Stadium" ~ "Los Angeles",
      "Guaranteed Rate Field" ~ "Chicago",
      "Kauffman Stadium" ~ "Kansas City",
      "Oracle Park" ~ "San Francisco",
      "Safeco Field" ~ "Seattle",
      "T-Mobile Park" ~ "Seattle",
      "Wrigley Field" ~ "Chicago",
      "U.S. Cellular Field" ~ "Chicago",
      "Yankee Stadium" ~ "New York",
      .default = NA_character_
    ),
    # Get time-zone from city
    tz = case_match(
      city,
      c("Chicago", "Kansas City", "St Louis") ~ "America/Chicago",
      "Detroit" ~ "America/Detroit",
      c("Los Angeles", "San Francisco", "Seattle") ~ "America/Los_Angeles",
      "New York" ~ "America/New_York",
      .default = NA_character_
    ),
    # Extract starting date-time
    # `game_date` gives the date-time in Zulu time, so we need to convert this
    # to local later
    date_time_utc = parse_date_time(game_date, orders = "Ymd T", tz = "UTC"),
    # If the game was a tie, set NA, otherwise TRUE if home team won, else FALSE
    home_win = if_else(is_tie, NA, teams_home_is_winner)
  ) |>
  # `with_tz` only accepts a single time-zone name, so we have to run it
  # separately on the data for each time zone
  mutate(date_time = with_tz(date_time_utc, tzone = tz), .by = tz) |>
  select(city, venue = venue_name, date_time, home_win) |>
  # Check data is in the format we expect (any deviation from these assertions
  # causes an error)
  verify(has_only_names("city", "venue", "date_time", "home_win")) |>
  verify(has_all_names("city", "venue", "date_time", "home_win")) |>
  assert(not_na, city, venue, date_time) |>
  assert(in_set(cities), city) |>
  assert(is.POSIXct, date_time) |>
  # Save data
  write_csv(here("analysis_data/events_mlb.csv.gz")) |>
  # Summarise events in each city each year
  count(year = year(date_time), city) |>
  pivot_wider(names_from = year, values_from = n)



# MAJOR LEAGUE SOCCER ----------------------------------------------------------

# Get data
rMLS::fixtures(2010, 2019) |>
  clean_names() |>
  filter(
    venue %in% c(
      "Banc of California Stadium", # LA FC (and occasional LA Galaxy)
      "CenturyLink Field", # Seattle Sounders
      "Citi Field", # NYC FC (occasional)
      "Yankee Stadium" # NYC FC
    )
  ) |>
  mutate(
    city = case_match(
      venue,
      "Banc of California Stadium" ~ "Los Angeles",
      "CenturyLink Field" ~ "Seattle",
      "Citi Field" ~ "New York",
      "Yankee Stadium" ~ "New York"
    ),
    # Parse game dates/times
    date_time = parse_datetime(str_glue("{date} {time}")),
    # Categorise win/lose/tie
    home_win = case_when(
      home_score > away_score ~ TRUE,
      home_score < away_score ~ FALSE,
      .default = NA
    )
  ) |>
  select(city, venue, date_time, home_win) |>
  # Check data is in the format we expect (any deviation from these assertions
  # causes an error)
  verify(has_only_names("city", "venue", "date_time", "home_win")) |>
  verify(has_all_names("city", "venue", "date_time", "home_win")) |>
  assert(not_na, city, venue, date_time) |>
  assert(in_set(cities), city) |>
  assert(is.POSIXct, date_time) |>
  # Save data
  write_csv(here("analysis_data/events_mls.csv.gz")) |>
  count(year = year(date_time), city) |>
  pivot_wider(names_from = year, values_from = n)




# NATIONAL FOOTBALL LEAGUE -----------------------------------------------------

# Get data (NFL seasons are over the winter, so we need data for 2009-10 too)
fast_scraper_schedules(2009:2019) |>
  filter(
    stadium %in% c(
      "Arrowhead Stadium", # Kansas City Chiefs
      "Candlestick Park", # San Francisco 49ers
      "CenturyLink Field", # Seattle Seahawks, from 2011
      "Edward Jones Dome", # St Louis Rams
      "Ford Field", # Detroit Lions
      "Los Angeles Memorial Coliseum", # LA Rams
      "Qwest Field", # Seattle Seahawks, until 2011
      "Soldier Field" # Chicago Bears
    )
  ) |>
  mutate(
    city = case_match(
      stadium,
      "Arrowhead Stadium" ~ "Kansas City",
      "Candlestick Park" ~ "San Francisco",
      "CenturyLink Field" ~ "Seattle",
      "Edward Jones Dome" ~ "St Louis",
      "Ford Field" ~ "Detroit",
      "Los Angeles Memorial Coliseum" ~ "Los Angeles",
      "Qwest Field" ~ "Seattle",
      "Soldier Field" ~ "Chicago",
      .default = NA_character_
    ),
    # Parse game dates/times
    date_time = parse_datetime(str_glue("{gameday} {gametime}")),
    # Categorise win/lose/tie
    home_win = case_when(
      location == "Home" & result > 0 ~ TRUE,
      location == "Home" & result < 0 ~ FALSE,
      .default = NA
    )
  ) |>
  filter(between(as_date(date_time), ymd("2010-01-01"), ymd("2019-12-31"))) |>
  select(city, venue = stadium, date_time, home_win) |>
  # Check data is in the format we expect (any deviation from these assertions
  # causes an error)
  verify(has_only_names("city", "venue", "date_time", "home_win")) |>
  verify(has_all_names("city", "venue", "date_time", "home_win")) |>
  assert(not_na, city, venue, date_time) |>
  assert(in_set(cities), city) |>
  assert(is.POSIXct, date_time) |>
  # Save data
  write_csv(here("analysis_data/events_nfl.csv.gz")) |>
  # Summarise events in each city each year
  count(year = year(date_time), city) |>
  pivot_wider(names_from = year, values_from = n)

