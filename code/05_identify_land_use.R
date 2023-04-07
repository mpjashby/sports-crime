# This code extracts land-use categories for 250-metre grid cells in each city
# and creates a geo-package for each city containing the grid cells that are
# primarily commercial, recreational or residential in nature.

# Load packages
library(sf)
library(stars)
library(tidyverse)


# Load data --------------------------------------------------------------------

# Get land use ----

# Download file if necessary
# Source: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LNBJIO
if (!file.exists(here("original_data/hisdac_land_use.tar.gz"))) {
  GET(
    url = "https://dataverse.harvard.edu/api/access/datafile/6191432",
    write_disk(here("original_data/hisdac_land_use.tar.gz")),
    progress(),
    timeout(60 * 60)
  )
}

# Unzip file if necessary
if (!file.exists(here("analysis_data/major_land_use.tif"))) {

  # Unzip file
  untar(
    tarfile = here("original_data/hisdac_land_use.tar.gz"),
    files = "Majority/LU_ThemeMaj_2015.tif",
    exdir = here("analysis_data")
  )

  # Move file
  file.copy(
    from = here("analysis_data/Majority/LU_ThemeMaj_2015.tif"),
    to = here("analysis_data/major_land_use.tif")
  )

  # Clean up
  unlink(here("analysis_data/Majority/"), recursive = TRUE)

}

# Load data
major_land_use <- read_stars(here("analysis_data/major_land_use.tif"))


# Get city boundaries ----
if (!file.exists(here("original_data/city_outlines.gpkg"))) {
  download.file(
    url = "https://github.com/mpjashby/crime-open-database/raw/master/spatial_data/city_outlines.gpkg",
    destfile = here("original_data/city_outlines.gpkg")
  )
}
city_boundaries <- read_sf(here("original_data/city_outlines.gpkg"))



# Extract cells ----------------------------------------------------------------

walk(
  c(
    "Austin", "Chicago", "Detroit", "Kansas City", "Los Angeles", "New York",
    "Seattle", "St Louis", "San Francisco", "Tucson"
  ),
  function(this_city) {

    # Get city boundary
    this_city_boundary <- city_boundaries |>
      filter(city_name == this_city) |>
      st_transform(st_crs(major_land_use, proj = TRUE))

    # Get name of city in snake_case
    this_city_file_name <- this_city |>
      str_replace_all("\\s", "_") |>
      str_to_lower()

    # Extract commercial, recreational and residential land-use cells
    major_land_use |>
      # Crop dataset to the city boundary
      st_crop(this_city_boundary) |>
      # Convert to an SF object because it's easier to work with
      st_as_sf() |>
      st_transform("EPSG:4326") |>
      rename(category = major_land_use.tif) |>
      # Filter categories
      # Source: https://dataverse.harvard.edu/file.xhtml?fileId=6192997&version=3.0
      filter(category %in% c(2, 4, 5, 6)) |>
      mutate(
        category = case_match(
          category,
          2 ~ "commercial",
          4 ~ "recreational",
          c(5, 6) ~ "residential"
        )
      )|>
      write_sf(
        here(
          str_glue("analysis_data/land_use_{this_city_file_name}.gpkg")
        )
      )

  }
)

