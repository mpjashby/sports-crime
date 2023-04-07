# This file downloads liquor-licence data from the various liquor licensing
# bodies of the states. In some cases liquor-licence data has to be extracted
# from a dataset of all business licences, while in other cases it is a separate
# dataset.
#
# This script should produce a single geopackage file for each city, with the
# file name starting with `licences_` and the file being stored in the
# `analysis_data` directory. The original downloaded files should be stored in
# the `original_data` directory, but since they are very large they will be
# automatically gitignored if the file name starts `licences_`.
#
# Some of the downloaded files are very large, so `httr::GET()` is used instead
# of `utils::download.file()`.



# Load packages
library(here)
library(httr)
library(janitor)
library(readxl)
library(sf)
library(tidygeocoder)
library(tidyverse)



# AUSTIN -----------------------------------------------------------------------

# Austin data is available only via web form at
# https://apps.tabc.texas.gov/publicinquiry/ -- this file has data for the city
# of Austin in Travis county, all zip codes, AIMS Tier licence types BE, BG, MB,
# N, NB and NE.
# List of licence type is at
# https://www.tabc.texas.gov/static/5a4c3688018ee81800cdbb5004db6096/public-inquiry-system-license-descriptions.pdf

# Extract liquor licences valid between 2010 and 2019
here("original_data/licences_austin.csv") |>
  read_csv() |>
  clean_names() |>
  mutate(
    across(
      c(original_issue_date, expiration_date),
      \(x) as_date(str_extract(x, "^\\d+/\\d+/\\d+"), format = "%m/%d/%Y")
    ),
    geo_address = str_to_upper(address),
    geo_address = str_replace(geo_address, " IH ", " INTERSTATE "),
    geo_address = str_replace(geo_address, " FM ", " FARM ROAD "),
    # Remove addresses with specific suites, units, etc. after the street
    # address
    geo_address = str_remove(
      geo_address,
      " (BLDG|BUILDING|INCLUDING|RM|SPACE|\\#|SUITE|STE|UNIT).+$"
    ),
    # Sometimes units, etc., are just shown as numbers in single quotes
    geo_address = str_remove(geo_address, " \\'\\w+\\'$"),
    geo_address = str_glue(
      "{geo_address}, {city}, {state} {str_sub(zip, end = 5)}"
    )
  ) |>
  filter(
    # Exclude licences that expired before Jan 2010
    expiration_date >= ymd("2010-01-01"),
    # Exclude licences that did not start until after Dec 2019
    original_issue_date <= ymd("2019-12-31")
  ) |>
  distinct(geo_address) |>
  geocode_combine(
    queries = list(list(method = "census"), list(method = "osm")),
    global_params = list(address = "geo_address")
  ) |>
  drop_na(long, lat) |>
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") |>
  write_sf(here("analysis_data/licences_austin.gpkg"))



# CHICAGO ----------------------------------------------------------------------

# Download data
GET(
  "https://data.cityofchicago.org/api/views/r5kz-chrr/rows.csv?accessType=DOWNLOAD",
  write_disk(here("original_data/businesses_chicago.csv")),
  progress(),
  timeout(60 * 60)
)

# Extract liquor licences valid between 2010 and 2019
here("original_data/licences_chicago.csv") |>
  read_csv() |>
  janitor::clean_names() |>
  mutate(
    across(
      c(license_term_start_date, license_term_expiration_date),
      \(x) as_date(x, format = "%m/%d/%Y")
    )
  ) |>
  filter(
    # Exclude licences that expired before Jan 2010
    license_term_expiration_date >= ymd("2010-01-01"),
    # Exclude licences that did not start until after Dec 2019
    license_term_start_date <= ymd("2019-12-31"),
    # Include only places with liquor licences
    license_description %in% c(
      "Consumption on Premises - Incidental Activity",
      "Tavern"
    )
  ) |>
  # Remove duplicates at the same address
  summarise(across(c(longitude, latitude), first), .by = address) |>
  drop_na(longitude, latitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
  write_sf(here("analysis_data/licences_chicago.gpkg"))



# DETROIT ----------------------------------------------------------------------

# Detroit data are contained in a state-wide liquor-licence file, which only
# contains current licences as of 23 March 2023. There doesn't appear to be
# an alternative source of historical data, or a field in the data saying when
# the licence began.

# Download data
# Source: https://www.michigan.gov/lara/bureau-list/lcc/licensing-list
GET(
  "https://www.michigan.gov/lara/-/media/Project/Websites/lara/lcc/License-Lists/Master-License-List.xlsx?rev=612dc94ac4864e67b42184119273bd75&hash=B93E8E16101ED851B6078126D8648A6D",
  write_disk(here("original_data/licences_detroit.xlsx")),
  progress(),
  timeout(60 * 60)
)

# Detroit data are contained in a state-wide liquor-licence file, which only
# contains current licences as of 23 March 2023. There doesn't appear to be
# an alternative source of historical data, or a field in the data saying when
# the licence began.
here("original_data/licences_detroit.xlsx") |>
  read_excel(skip = 1) |>
  clean_names() |>
  filter(
    current_lgu_lgu_name == "DETROIT CITY",
    group == "Retail - On Premises"
  ) |>
  distinct(address) |>
  # Remove numeric ranges from addresses, since geocoding needs to be to a
  # specific address
  mutate(address = str_remove_all(address, "-\\d+\\b")) |>
  geocode_combine(
    queries = list(list(method = "census"), list(method = "osm")),
    global_params = list(address = "address")
  ) |>
  drop_na(long, lat) |>
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") |>
  write_sf(here("analysis_data/licences_detroit.gpkg"))



# KANSAS CITY ------------------------------------------------------------------

# Missouri only has current data (as of 30 March 2023). There is a location
# (lon/lat) column in the data, but it is mostly missing values. Licence type
# list is at https://atc.dps.mo.gov/fees/LiquorCodes.pdf

# Download data
# Source: https://data.mo.gov/Regulatory/Missouri-Active-Alcohol-License-Data/yyhn-562y
GET(
  "https://data.mo.gov/api/views/yyhn-562y/rows.csv?accessType=DOWNLOAD",
  write_disk(here("original_data/licences_kansas_city.csv")),
  progress(),
  timeout(60 * 60)
)

# Extract current bars
here("original_data/licences_kansas_city.csv") |>
  read_csv() |>
  clean_names() |>
  filter(
    # Keep only premises in Kansas City
    city == "KANSAS CITY",
    # Keep only on-premises licences
    primary_type %in% c("5BD", "5BDW", "COL", "RBD", "RBDE")
  ) |>
  mutate(
    address = str_glue("{street_number}, {street}, {city}, {state}, {zipcode}")
  ) |>
  distinct(address) |>
  geocode_combine(
    queries = list(list(method = "census"), list(method = "osm")),
    global_params = list(address = "address")
  ) |>
  drop_na(long, lat) |>
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") |>
  write_sf(here("analysis_data/licences_kansas_city.gpkg"))



# LOS ANGELES ------------------------------------------------------------------

# CA liquor-licence data is only available via a web form at
# https://www.abc.ca.gov/licensing/licensing-reports/licenses-by-city/ so the
# following file has been downloaded (on 30 March 2023) for on-premises liquor
# licences in LA City. Licence type codes are listed at
# https://www.abc.ca.gov/licensing/license-types/

# Extract current liquor licences
here("original_data/licences_los_angeles.csv") |>
  read_csv() |>
  clean_names() |>
  mutate(
    across(
      c(orig_iss_date, expir_date),
      \(x) as_date(x, format = "%m/%d/%Y")
    )
  ) |>
  filter(
    # Exclude licences that expired before Jan 2010
    expir_date >= ymd("2010-01-01"),
    # Exclude licences that did not start until after Dec 2019
    orig_iss_date <= ymd("2019-12-31"),
    # Exclude catering licences (type 58), hotel in-room mini-bars (type 66),
    # special-event permits (type 77) and instructional tasting (type 86)
    !license_type %in% c(58, 66, 77, 86)
  ) |>
  # Remove census tract ID from end of address
  mutate(
    premises_addr = str_squish(
      str_replace(str_remove(premises_addr, "Census Tract.+$"), ",", ", ")
    )
  ) |>
  distinct(premises_addr) |>
  geocode_combine(
    queries = list(list(method = "census"), list(method = "osm")),
    global_params = list(address = "premises_addr")
  ) |>
  drop_na(long, lat) |>
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") |>
  write_sf(here("analysis_data/licences_los_angeles.gpkg"))



# NEW YORK CITY ----------------------------------------------------------------

# Download data
# Source: https://data.ny.gov/Economic-Development/Liquor-Authority-Quarterly-List-of-Active-Licenses/hrvs-fxs2/data
GET(
  "https://data.ny.gov/api/views/hrvs-fxs2/rows.csv?accessType=DOWNLOAD&sorting=true",
  write_disk(here("original_data/licences_new_york.csv")),
  progress(),
  timeout(60 * 60)
)

here("original_data/licences_new_york.csv") |>
  read_csv() |>
  clean_names() |>
  mutate(
    across(
      c(original_date, license_expiration_date),
      \(x) as_date(x, format = "%m/%d/%Y")
    ),
    license_class_code = parse_number(license_class_code)
  ) |>
  filter(
    # Include only NYC licences
    premise_city == "NEW YORK",
    # Exclude licences that did not start until after Dec 2019
    original_date <= ymd("2019-12-31"),
    # Exclude off-premises and wholesale licences
    !license_class_code %in% c(
      # Off-premises licences
      121, 122, 123, 124, 126, 128, 222, 305, 322,
      # Wholesale/manufacturing licences
      101, 102, 103, 104, 105, 201, 202, 203, 204, 205, 206, 301, 302, 303, 304
    )
  ) |>
  select(geometry = georeference) |>
  drop_na() |>
  mutate(geometry = st_as_sfc(geometry)) |>
  st_as_sf(crs = "EPSG:4326") |>
  write_sf(here("analysis_data/licences_new_york.gpkg"))



# ST LOUIS ---------------------------------------------------------------------

# There is a location (lon/lat) column in the data, but it is mostly missing
# values. Licence type list is at https://atc.dps.mo.gov/fees/LiquorCodes.pdf

# Download data
# Source: https://data.mo.gov/Regulatory/Missouri-Active-Alcohol-License-Data/yyhn-562y
GET(
  "https://data.mo.gov/api/views/yyhn-562y/rows.csv?accessType=DOWNLOAD",
  write_disk(here("original_data/licences_st_louis.csv")),
  progress(),
  timeout(60 * 60)
)

# Extract current liquor licences
here("original_data/licences_st_louis.csv") |>
  read_csv() |>
  clean_names() |>
  filter(
    # Keep only St Louis licences
    city == "ST. LOUIS",
    # Keep only on-premises licences
    primary_type %in% c("5BD", "5BDW", "COL", "RBD", "RBDE")
  ) |>
  mutate(
    address = str_glue("{street_number}, {street}, {city}, {state}, {zipcode}")
  ) |>
  distinct(address) |>
  geocode_combine(
    queries = list(list(method = "census"), list(method = "osm")),
    global_params = list(address = "address")
  ) |>
  drop_na(long, lat) |>
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") |>
  write_sf(here("analysis_data/licences_st_louis.gpkg"))



# SAN FRANCISCO ----------------------------------------------------------------

# CA liquor-licence data is only available via a web form at
# https://www.abc.ca.gov/licensing/licensing-reports/licenses-by-city/ so the
# following file has been downloaded (on 30 March 2023) for on-premises liquor
# licences in San Francisco. Licence type codes are listed at
# https://www.abc.ca.gov/licensing/license-types/

# Extract current liquor licences
here("original_data/licences_san_francisco.csv") |>
  read_csv() |>
  clean_names() |>
  mutate(
    across(
      c(orig_iss_date, expir_date),
      \(x) as_date(x, format = "%m/%d/%Y")
    )
  ) |>
  filter(
    # Exclude licences that expired before Jan 2010
    expir_date >= ymd("2010-01-01"),
    # Exclude licences that did not start until after Dec 2019
    orig_iss_date <= ymd("2019-12-31"),
    # Exclude catering licences (type 58), hotel in-room mini-bars (type 66),
    # special-event permits (type 77) and instructional tasting (type 86)
    !license_type %in% c(58, 66, 77, 86)
  ) |>
  mutate(
    premises_addr = str_squish(
      str_replace(str_remove(premises_addr, "Census Tract.+$"), ",", ", ")
    )
  ) |>
  distinct(premises_addr) |>
  geocode_combine(
    queries = list(list(method = "census"), list(method = "osm")),
    global_params = list(address = "premises_addr")
  ) |>
  drop_na(long, lat) |>
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") |>
  write_sf(here("analysis_data/licences_san_francisco.gpkg"))



# SEATTLE ----------------------------------------------------------------------

# Download data
# Source: https://lcb.wa.gov/records/frequently-requested-lists
# Note the website seems to have some restriction on deep linking, so it may be
# necessary to include a referrer header in this query
GET(
  "https://lcb.wa.gov/sites/default/files/publications/Public_Records/2019/On%20Premise03282023.xlsx",
  write_disk(here("original_data/licences_seattle.xlsx")),
  progress(),
  timeout(60 * 60)
)

# Get current licences for businesses active from 2010 to 2019
here("original_data/licences_seattle.xlsx") |>
  read_excel() |>
  clean_names() |>
  mutate(
    across(
      c(business_startup_date, expire_date),
      \(x) as_date(as.character(x), format = "%Y%m%d")
    )
  ) |>
  filter(
    # Keep only Tucson licences
    loc_city == "SEATTLE",
    # Exclude licences that expired before Jan 2010
    expire_date >= ymd("2010-01-01"),
    # Exclude licences that did not start until after Dec 2019
    business_startup_date <= ymd("2019-12-31"),
  ) |>
  mutate(
    address = str_glue(
      "{loc_address}, {loc_city}, {loc_st} {str_sub(loc_zip, end = 5)}"
    )
  ) |>
  distinct(address) |>
  geocode_combine(
    queries = list(list(method = "census"), list(method = "osm")),
    global_params = list(address = "address")
  ) |>
  drop_na(long, lat) |>
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") |>
  write_sf(here("analysis_data/licences_seattle.gpkg"))



# TUCSON -----------------------------------------------------------------------

# Licence types are at
# https://azliquor.gov/assets/documents/Annual%20Report/18_annualrpt.pdf

# Download data
# Source: https://www.azliquor.gov/query/query.cfm#DT
GET(
  "https://www.azliquor.gov/query/master.csv",
  write_disk(here("original_data/licences_tucson.csv")),
  progress(),
  timeout(60 * 60)
)

# Extract liquor licences from 2010 to 2019
here("original_data/licences_tucson.csv") |>
  read_csv() |>
  clean_names() |>
  mutate(
    across(
      c(issued_date, exp_date),
      \(x) as_date(x, format = "%m/%d/%Y")
    )
  ) |>
  filter(
    # Keep only Tucson licences
    city == "TUCSON",
    # Exclude licences that expired before Jan 2010
    exp_date >= ymd("2010-01-01"),
    # Exclude licences that did not start until after Dec 2019
    issued_date <= ymd("2019-12-31"),
    # Keep only on-premises licences
    lic_type %in% c(
      "006", "007", "011", "012", "12G", "013", "014", "COO", "UNL"
    )
  ) |>
  # Some rows of `business_address` contain unit numbers after the address. We
  # need to keep these to count them as separate addresses, but they won't work
  # with the geocoder, so we need to remove them from the data sent to the
  # geocoder.
  distinct(business_address, city, zip) |>
  mutate(
    address = str_squish(
      str_glue("{str_remove(business_address, '#.+$')}, {city}, AZ {zip}")
    )
  ) |>
  geocode_combine(
    queries = list(list(method = "census"), list(method = "osm")),
    global_params = list(address = "address")
  ) |>
  drop_na(long, lat) |>
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326") |>
  write_sf(here("analysis_data/licences_tucson.gpkg"))
