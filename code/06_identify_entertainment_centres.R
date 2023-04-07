# This script identifies the main entertainment district in each city, based on
# liquor-licensing data and the method explained at
# https://www.jstor.org/stable/48657957

# Before identifying entertainment districts, this code first uses land-use data
# from https://doi.org/10.7910/DVN/LNBJIO to exclude any parts of the city that
# are not commercial, recreational or residential. This may be particularly
# important for cities with industrial areas on the edge of the downtown area.
# This data has been pre-processed in the code/05_identify_land.R file.



# Load packages
library(here)
library(httr)
library(sf)
library(sfhotspot)
library(smoothr)
library(tidyverse)



#  Load data -------------------------------------------------------------------

# Get city boundaries ----
if (!file.exists(here("original_data/city_outlines.gpkg"))) {
  download.file(
    url = "https://github.com/mpjashby/crime-open-database/raw/master/spatial_data/city_outlines.gpkg",
    destfile = here("original_data/city_outlines.gpkg")
  )
}
city_boundaries <- read_sf(here("original_data/city_outlines.gpkg"))



# Identify main entertainment district in each city ----------------------------

ent_districts <- here("analysis_data") |>
  dir(pattern = "^licences_", full.names = TRUE) |>
  map(function(file) {

    # Extract city name
    this_city_name <- file |>
      str_remove(here("analysis_data/licences_")) |>
      str_remove("\\.gpkg$") |>
      str_replace_all("_", " ") |>
      str_to_title()

    # Extract city boundary
    city_boundary <- filter(city_boundaries, city_name == this_city_name)

    # Find best CRS for this city
    city_crs <- crsuggest::suggest_top_crs(city_boundary)

    # Load land-use data
    city_land <- file |>
      str_replace("licences_", "land_use_") |>
      read_sf() |>
      st_transform(city_crs)

    # Create 150m hex grid for city
    city_grid <- city_land |>
      # `hotspot_grid()` needs a projected CRS
      st_transform(city_crs) |>
      hotspot_grid(cell_size = 150, grid_type = "hex") |>
      # Create row number column to make joining easier
      mutate(row = row_number())

    # Join land use to hex-grid centroids
    city_grid_centroids <- city_grid |>
      st_centroid() |>
      # Join land-use types to centroids of grid cells
      st_join(city_land) |>
      # Remove geometry column for joining back to `city_grid` object
      st_drop_geometry()

    # Create outline of grid cells that are commercial, recreational or
    # residential
    city_grid_outline <- city_grid |>
      left_join(city_grid_centroids, by = "row") |>
      select(category) |>
      # Remove any grid cells that are not in commercial, recreational or
      # residential areas (this must be done after `hotspot_kde()` to avoid
      # holes in the KDE surface during calculation)
      filter(!is.na(category)) |>
      # Convert to a single multipolygon showing the outline
      st_union() |>
      st_as_sf()

    # Load point data for city
    city_licences <- read_sf(file)

    # Find grid cells with the highest density
    city_hotspots <- city_licences |>
      # `hotspot_kde()` needs a projected CRS
      st_transform(city_crs) |>
      hotspot_kde(bandwidth_adjust = 0.5, grid = city_grid) |>
      # `hotspot_grid()` creates a grid based on the convex hull of the input
      # data, so clip that to the city boundary
      st_intersection(city_grid_outline) |>
      # `st_intersection()` creates some lines at the edges of the city, so we
      # remove these by only keeping rows with POLYGON geometry
      filter(st_geometry_type(geometry) == "POLYGON") |>
      # Arrange cells in descending order of density and keep the cells with
      # 10% of the total density
      arrange(desc(kde)) |>
      mutate(cumulative_percent = cumsum(kde) / sum(kde)) |>
      filter(cumulative_percent <= 0.1)

    # Merge adjacent grid cells
    city_hotspot_groups <- city_hotspots |>
      # Union all grid cells into a single multi-polygon containing a separate
      # polygon for each set of non-adjacent cells
      st_union() |>
      # Cast that multi-polygon to a polygon, which extracts each polygon
      # representing a group of non-adjacent cells into a separate polygon
      # feature
      st_cast(to = "POLYGON") |>
      # `st_cast()` produces an SFC object, so convert back to SF so we can use
      # it with dplyr functions
      st_as_sf() |>
      st_set_geometry("geometry") |>
      mutate(group = row_number())

    # Find group of adjacent cells with the highest total density
    city_largest_group <- city_hotspots |>
      # Join hotspot group numbers to individual cells
      st_join(city_hotspot_groups) |>
      # Convert to a tibble to make `count()` faster, since we don't need the
      # geometry
      st_drop_geometry() |>
      # Sum total density in each group of grid cells
      count(group, wt = kde, sort = TRUE) |>
      # Get group number for group with highest total density
      pluck("group", 1)

    # Return generalised boundary of group with highest total density
    city_hotspot_groups |>
      filter(group == city_largest_group) |>
      mutate(city = this_city_name) |>
      # Slightly increase area covered, to ensure all cells are covered when the
      # boundary is generalised
      st_buffer(150 / 2) |>
      # Simplify the boundary so that the smoothing is not influenced by the
      # use of a hex grid
      st_simplify(dTolerance = 150 / 4) |>
      # Remove any holes in the area covered, since these tend to be artefacts
      # of using grid cells
      fill_holes(threshold = Inf) |>
      # Smooth the boundary to reduce the effects of the arbitrary choice of
      # grid size, shape, etc.
      smooth(method = "ksmooth", smoothness = 2) |>
      select(city, geometry) |>
      st_transform("EPSG:4326")

  }) |>
  bind_rows() |>
  write_sf(here("analysis_data/entertainment_districts.gpkg"))

