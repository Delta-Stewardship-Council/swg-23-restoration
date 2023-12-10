# This script downloads details of projects.
# For more information, see intro_to_ecoatlas.qmd in /documentation.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up ----
# Load packages

# Load path to cloud directory
source("admin_scripts/init_load_paths.R")

# Load functions for this scrape.
source(file.path(path_home,
                 "data_processing", 
                 "ecoatlas", 
                 "funs_ecoatlas.R"))

# create paths for output
path_ecodata <- file.path(path_data, 
                          "scraped_data",
                          "ecoatlas")

path_listings <- file.path(
  path_ecodata,
  "project_listings"
)

path_details <- file.path(
  path_ecodata,
  "project_details"
)

path_json <- file.path(
  path_details,
  "raw_json_files"
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Conditionally create directories.
# Make sure all the paths exist
check_dir(path_details)
check_dir(path_json)

# Load listing of projects
listings <- read_csv(file.path(path_listings, "bay_delta_plus_project_listings.csv"))

# get unique ids
ids <- listings$projectid %>%
  unique()

# REST api execution
jsons <- get_projs(
    .ids = ids,
    .dir = path_json,
    .agent = "NCEAS Restoration Group"
)