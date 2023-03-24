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

# Load sports API packages
library(baseballr)
library(rMLS)
library(nbastatR)
library(nflfastR)
library(nhlapi)

# Load data-wrangling packages
library(assertr)
library(here)
library(janitor)
library(tidyverse)

# Create vector of cities we can later use to check against
cities <- c(
  "Austin", "Chicago", "Detroit", "Kansas City", "Los Angeles", "Louisville",
  "Memphis", "New York", "Seattle", "St Louis", "San Francisco", "Tucson"
)



# MAJOR LEAGUE BASEBALL --------------------------------------------------------

# Create vector of teams names we can later use to check against
teams_baseball <- teams_lu_table |>
  filter(sport.id == 1) |>
  pull("name")

# The `team_results_bref()` function from `baseballr` returns all the
# information we need if we supply a team abbreviation and a year
expand_grid(
  Tm = c("CHC", "CHW", "DET", "KCR", "LAD", "NYM", "NYY", "SFG", "SEA", "STL"),
  year = 2010:2019
) |>
  # Get data for each team for each year
  pmap(team_results_bref) |>
  bind_rows() |>
  clean_names() |>
  # Filter so we only keep games at the team's home stadium
  filter(h_a == "H") |>
  # Add city and team names from reference table
  left_join(
    select(
      # Filter so that the reference table includes on MLB teams, since the full
      # reference table also contains information for minor league teams, some
      # of which share an abbreviation with an MLB team
      filter(teams_lu_table, sport.id == 1),
      city = locationName,
      team = name,
      tm = bref_abbreviation
    ),
    by = "tm"
  ) |>
  mutate(
    # Recode `city` to remove "." from "St. Louis" to match elsewhere
    city = case_match(
      city,
      "Bronx" ~ "New York",
      "St. Louis" ~ "St Louis",
      .default = city
    ),
    # Remove numbers in brackets after dates on days with multiple games
    date = str_remove(date, "\\s\\(.+?\\)"),
    # Parse game dates/times
    date_time = parse_date_time(
      str_glue("{date} {year} {time}pm"),
      orders = "%a %b %d %Y %I:%M%p"
    ),
    # Walkoff wins/losses are noted in the `result` column, but we aren't
    # interested in whether a win/loss was a walkoff or not, so we can remove
    # this information
    result = str_sub(result, end = 1)
  ) |>
  select(city, team, date_time, result, attendance) |>
  # Check data is in the format we expect (any deviation from these assertions
  # causes an error)
  verify(has_only_names("city", "team", "date_time", "result", "attendance")) |>
  verify(has_all_names("city", "team", "date_time", "result")) |>
  assert(not_na, city, team, date_time, result) |>
  assert(in_set(cities), city) |>
  assert(in_set(teams_baseball), team) |>
  assert(is.POSIXct, date_time) |>
  assert(in_set("W", "L", "T"), result) |>
  # There are a few missing values for attendance figures, so we won't stop when
  # we find these but instead issue a warning as a reminder of how many are
  # missing
  assert(not_na, attendance, error_fun = just_warn) |>
  assert(within_bounds(1, 120000), attendance, error_fun = just_warn) |>
  # Save data
  write_csv(here("analysis_data/events_mlb.csv.gz")) |>
  glimpse()



# MAJOR LEAGUE SOCCER ----------------------------------------------------------

# Create vector of teams names we can later use to check against
teams_mls <- team_info |>
  filter(team_abbr %in% c("CHI", "LAFC", "NYC", "SEA")) |>
  pull("team_name")

# Get data
events_mls <- rMLS::fixtures(2010, 2019) |>
  clean_names() |>
  filter(
    home %in% c(
      "Chicago Fire FC", "Los Angeles FC", "New York City FC",
      "Seattle Sounders FC"
    )
  ) |>
  mutate(
    city = case_match(
      home,
      "Chicago Fire FC" ~ "Chicago",
      "Los Angeles FC" ~ "Los Angeles",
      "New York City FC" ~ "New York",
      "Seattle Sounders FC" ~ "Seattle"
    ),
    # Parse game dates/times
    date_time = parse_datetime(str_glue("{date} {time}")),
    # Categorise win/lose/tie
    result = case_when(
      home_score > away_score ~ "W",
      home_score < away_score ~ "L",
      home_score == away_score ~ "T",
      .default = NA_character_
    )
  ) |>
  select(city, team = home, date_time, result, attendance) |>
  # Check data is in the format we expect (any deviation from these assertions
  # causes an error)
  verify(has_only_names("city", "team", "date_time", "result", "attendance")) |>
  verify(has_all_names("city", "team", "date_time", "result", "attendance")) |>
  assert(not_na, city, team, date_time, result) |>
  assert(in_set(cities), city) |>
  assert(in_set(teams_mls), team) |>
  assert(is.POSIXct, date_time) |>
  assert(in_set("W", "L", "T"), result) |>
  # There are a few missing values for attendance figures, so we won't stop when
  # we find these but instead issue a warning as a reminder of how many are
  # missing
  assert(not_na, attendance, error_fun = just_warn) |>
  assert(within_bounds(1, 120000), attendance, error_fun = just_warn) |>
  # Save data
  write_csv(here("analysis_data/events_mls.csv.gz")) |>
  glimpse()




# NATIONAL FOOTBALL LEAGUE -----------------------------------------------------

# Create vector of teams names we can later use to check against
teams_nfl <- teams_colors_logos |>
  filter(team_abbr %in% c("CHI", "DET", "KC", "LA", "SEA")) |>
  pull("team_name")

# Get data
fast_scraper_schedules(2010:2019) |>
  filter(home_team %in% c("CHI", "DET", "KC", "LA", "SEA")) |>
  # Rename team abbreviation column to allow join
  rename(team_abbr = home_team) |>
  left_join(
    select(teams_colors_logos, team_abbr, team_name),
    by = "team_abbr"
  ) |>
  mutate(
    city = case_match(
      team_abbr,
      "CHI" ~ "Chicago",
      "DET" ~ "Detroit",
      "KC" ~ "Kansas City",
      "LA" ~ "Los Angeles",
      "SEA" ~ "Seattle",
      .default = NA_character_
    ),
    # Parse game dates/times
    date_time = parse_datetime(str_glue("{gameday} {gametime}")),
    # Categorise win/lose/tie
    result = case_when(
      result > 0 ~ "W",
      result < 0 ~ "L",
      result == 0 ~ "T",
      .default = NA_character_
    )
  ) |>
  select(city, team = team_name, date_time, result) |>
  # Check data is in the format we expect (any deviation from these assertions
  # causes an error)
  verify(has_only_names("city", "team", "date_time", "result")) |>
  verify(has_all_names("city", "team", "date_time", "result")) |>
  assert(not_na, city, team, date_time, result) |>
  assert(in_set(cities), city) |>
  assert(in_set(teams_nfl), team) |>
  assert(is.POSIXct, date_time) |>
  assert(in_set("W", "L", "T"), result) |>
  # Save data
  write_csv(here("analysis_data/events_nfl.csv.gz")) |>
  glimpse()

