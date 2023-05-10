# This script:
#   * Extracts the names of stadia in the event data and geocodes the names.
#   * Searches the OSM database for stadia around the geocoded addresses and
#     extracts the stadium outlines.
#   * Combines those stadium outlines into venue complexes if stadia are nearby
#     to one another or if they are actually the same stadium but have been
#     re-named over time.
#   * Re-names stadia that have a sponsorship name that is different from their
#     common name.
#   * Calculates a 1km buffer around each venue complex and saves the result.



# Load packages ----------------------------------------------------------------

library(crsuggest)
library(here)
library(osmdata)
library(sf)
library(tidygeocoder)
library(tidyverse)



# Geocode venue locations ------------------------------------------------------

venue_locations <- here("analysis_data") |>
  dir("^events_", full.names = TRUE) |>
  map(read_csv, show_col_types = FALSE) |>
  bind_rows() |>
  count(city, venue) |>
  select(city, venue) |>
  # Remove Candlestick Park because it was demolished in 2014 and has wildly
  # incorrect co-ordinates in the OSM database, so remove this and add it back
  # later with manual co-ordinates
  filter(venue != "Candlestick Park") |>
  mutate(
    # Some venues have been re-named over time. We need the various names to
    # match to the event data, but in some cases only the current name can be
    # geocoded, so we need to recode to the current name while keeping the
    # original name.
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
    address = str_glue("{venue}, {city}")
  ) |>
  count(city, venue, address) |>
  geocode(address = address) |>
  select(city, venue, long, lat) |>
  # Add Candlestick Park manually
  add_row(
    city = "San Francisco",
    venue = "Candlestick Park",
    long = -122.386111,
    lat = 37.713611
  ) |>
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326")



# Get venue outlines from OSM --------------------------------------------------

venue_outlines <- venue_locations |>
  nest(location = -c(city, venue)) |>
  pmap(function(city, venue, location) {

    places <- location |>
      # Convert back to SF (`nest()` silently converts to tibble)
      st_as_sf() |>
      # Transform to a metre-based CRS
      st_transform(suggest_top_crs(st_as_sf(location))) |>
      # Create a 1km buffer
      st_buffer(1000) |>
      # Transform back (since `opq()` needs WGS84 co-ordinates)
      st_transform("EPSG:4326") |>
      # Extract bbox
      st_bbox() |>
      # Setup OPQ query
      opq() |>
      # Specify only stadium features
      add_osm_feature(key = "leisure", value = "stadium") |>
      # Extract results as SF
      osmdata_sf() |>
      # Remove any polygons that are parts of multipolygons
      unique_osmdata() |>
      # Remove any elements that are NULL (this also converts from an osmdata
      # object to an ordinary list)
      compact()

    # Extract only polygons and multipolygons (combining where necessary)
    if (all(c("osm_polygons", "osm_multipolygons") %in% names(places))) {
      location_polygons <- bind_rows(
        pluck(places, "osm_polygons"),
        pluck(places, "osm_multipolygons")
      )
    } else if ("osm_polygons" %in% names(places)) {
      location_polygons <- pluck(places, "osm_polygons")
    } else if ("osm_multipolygons" %in% names(places)) {
      location_polygons <- pluck(places, "osm_multipolygons")
    } else {
      location_polygons <- tibble(venue = venue)
    }

    # Extract the stadium outline (or return the geocoded address if no rows
    # were returned from the OSM database)
    if ("name" %in% names(location_polygons)) {

      location_polygons |>
        # Find the OSM feature with the name that is closest to the venue name
        mutate(dist = stringdist::stringdist(venue, name)) |>
        # Keep only the row with the closest name (if more than one row has this
        # name, all those rows will be kept)
        slice_min(dist, n = 1) |>
        # Remove all the OSM tags
        select(geometry) |>
        # If there are multiple rows with the same name, merge them into a
        # single multipolygon
        summarise() |>
        # Add the city and venue name
        mutate(city = city, venue = venue)

    } else {

      location |>
        # Add the city and venue name
        mutate(city = city, venue = venue)

    }

  }) |>
  bind_rows()



# Calculate buffers around venues ----------------------------------------------

venue_buffers <- venue_outlines |>
  # This `mutate()` call does three things:
  #   1. Combines nearby venues (i.e. those inside the 1km buffer of another
  #      venue) into venue complexes, which will be treated as one venue in the
  #      models. If complexes have names on the ground, those are used,
  #      otherwise they're named after the local neighbourhood or a main street.
  #   2. Creates complexes for venues with names that changed over the period of
  #      the data.
  #   3. Renames venues that are in the data with current (typically sponsored)
  #      names but are widely known by other names
  mutate(
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
  group_by(city, complex) |>
  summarise() |>
  ungroup() |>
  # Buffers need to be specified in metres, which means we need to first
  # transform to a suitable projected co-ordinate system. This needs to be done
  # separately for each row (because the cities are so far apart from one
  # another), so we will first nest the data so we can perform the operation
  # separately by row.
  nest(cols = geometry) |>
  mutate(
    cols = map(cols, function(x) {
      x |>
        st_transform(suggest_top_crs(x)) |>
        st_buffer(1000) |>
        st_transform("EPSG:4326")
    })
  ) |>
  unnest(cols = cols) |>
  # Convert back to SF (`nest()` silently converts to tibble)
  st_as_sf() |>
  select(city, complex, geometry) |>
  write_sf(here("analysis_data/venue_buffers.gpkg")) |>
  glimpse()



# Create maps showing the buffer around each venue complex ---------------------

venue_buffers |>
  nest(cols = geometry) |>
  mutate(row = row_number()) |>
  pwalk(function(city, complex, row, cols) {

    venue_plot <- cols |>
      st_as_sf() |>
      ggplot() +
      ggspatial::annotation_map_tile(zoomin = 1, quiet = TRUE) +
      geom_sf(fill = NA, linewidth = 1) +
      labs(title = complex) +
      theme_void()

    ggsave(
      here(str_glue("outputs/venue_map_{row}.jpg")),
      venue_plot,
      width = 2000,
      height = 2000,
      units = "px"
    )

})
