# This script extracts the names of stadia in the event data and geocodes the
# stadium locations.

# Load packages
library(crsuggest)
library(here)
library(sf)
library(tidygeocoder)
library(tidyverse)

# Extract venue locations
venue_locations <- here("analysis_data") |>
  dir("^events_", full.names = TRUE) |>
  map(read_csv, show_col_types = FALSE) |>
  bind_rows() |>
  count(city, venue) |>
  select(city, venue) |>
  mutate(
    # Some venues have been re-named over time. We need the various names to
    # match to the event data, but only the current name can be geocoded, so
    # we need to recode to the current name while keeping the original name.
    geocode_venue_name = case_match(
      venue,
      "U.S. Cellular Field" ~ "Guaranteed Rate Field",
      "Banc of California Stadium" ~ "BMO Stadium",
      "Edward Jones Dome" ~ "The Dome at America's Center",
      .default = venue
    ),
    address = str_glue("{geocode_venue_name}, {city}")
  ) |>
  geocode(address = address) |>
  select(city, venue, long, lat) |>
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") |>
  write_sf(here("analysis_data/venue_locations.gpkg"))

# Calculate buffers around venues
venue_locations |>
  # Buffers need to be specified in metres, which means we need to first
  # transform to a suitable projected co-ordinate system. This needs to be done
  # separately for each row, so we will first nest the data so we can perform
  # the operation separately by row.
  mutate(row = row_number()) |>
  nest(cols = -row) |>
  mutate(
    cols = map(cols, function(x) {
      x |>
        st_transform(suggest_top_crs(x)) |>
        st_buffer(1000) |>
        st_transform("EPSG:4326")
    })
  ) |>
  unnest(cols = cols) |>
  select(city, venue, geometry) |>
  write_sf(here("analysis_data/venue_buffers.gpkg"))
