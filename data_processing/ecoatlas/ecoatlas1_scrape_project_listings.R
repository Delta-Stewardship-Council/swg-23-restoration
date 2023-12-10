# This script downloads a listing of projects, but not the project details.
# For more information, see intro_to_ecoatlas.qmd in /documentation.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up ----

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

# path_details <- file.path(path_ecodata, "project_details")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Conditionally create directories.
# Make sure all the paths exist

check_dir(path_ecodata)
check_dir(path_listings)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load query source list
# This list contains all the ways to search ecoatlas, and should be
# comprehensive.

sources <- file.path(path_home,
          "data_processing", 
          "ecoatlas", 
          "source_list.csv") %>%
  read_csv()

# remove proj lists we don't want to query.
sources <- sources %>%
  filter(`Download?` == T)

# Download listings
listings <- get_listings(
  .tb = sources[,c("TypeKey", "key")],
  .agent = "NCEAS Restoration Group"
  )

# Store to cloud
write_csv(
  x = listings,
  file = file.path(
    path_listings,
    "bay_delta_plus_project_listings.csv"
  )
)
