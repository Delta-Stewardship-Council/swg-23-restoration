# R version 4.3.1 (2023-06-16)
# CNRA (Bond Accountability) Scraping Script 6.
# Author: KT
# Purpose: This script validates the data we downloaded.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# initialize libs, paths, etc.
source("data_processing/cnra_scrape/init_cnra_scrape.R")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Verify Logged Spatial Data Correctly Downloaded
# Are all spatial geometries downloaded?
# Answer: No, but all available geometries were downloaded!
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
existing_sp <- list.files(path_spatial, pattern = "\\.geojson$")

# extract project id's
existing_sp <- existing_sp %>%
  str_extract(., "^\\d+(?=\\_)")

# load csv
spd <- read_csv(file.path(path_spatial, "spatial_directory.csv"))

# Missing Spatial
spd <- spd %>%
  mutate(has_geom = case_when(
    POLYGON == 0 & LINESTRING == 0 & POINT == 0 ~ F,
    .default = T
    ))

# Note, there are four projects with no geometries.
# KT manually verified that the data is missing by checking the project
# website (Nov 3, 2023). It may be valuable to re-check at a later date.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Are all projects accounted for in spd?
# Answer: Yes!
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

meta_proj <- read_csv(file.path(path_proj, "metadata.csv"))

# Which spatial geometries are missing according to the metadata on projects?
all_proj <- meta_proj$project_id %>% 
  unique()

# All projects in spatial geometries are accounted for,
# because the following code is TRUE.
all(spd$project_id == all_proj)
