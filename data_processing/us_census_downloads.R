# This script downloads and manipulates US Census data to meet the needs of
# this project. We do operations that require downloads or computationally
# expensive here so as not to continually repeat these processes.
# All data are downloaded to our cloud data directory under US_CENSUS.

# Load path to cloud directory
source("admin_scripts/init_load_paths.R")

# load libraries
load_libs(c("tidyverse", "sf", "tigris"))

path_census <- file.path(
  path_data,
  "US_CENSUS"
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Raw Census Data ----

## California County Geometries ----

# When the following function executes, you may notice a warning which
# describes an issue with "polygon(s) with rings with invalid winding 
# order." As best as I can tell, this is corrected before we write it to file.
ca_co <- tigris::counties(state = "06", 
                          year = 2021)
sf::st_write(
  obj = ca_co,
  dsn = file.path(path_census,
                  "ustiger_raw_data.gpkg"),
  layer = "06counties2021"
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project Manipulated Geometries ----
# Data in this section have their CRS transformed to the project CRS,
# and in subsetted to match the boundaries of our project.

# NCEAS Restoration Project Sampling Frame/Spatial Boundary
# Load our bay-delta boundary
bdb <- st_read(file.path(
  path_data,
  "bay_delta_boundary",
  "Bay_EcoLegalDelta.shp"
))

co_sf <- ca_co %>%
  st_transform(crs = crs_$crs)

co_sf <- co_sf[bdb,, op = st_intersects]

## Project County Geometries ----
sf::st_write(
  obj = co_sf,
  dsn = file.path(path_census,
                  "ustiger_project_data.gpkg"),
  layer = "counties"
)
