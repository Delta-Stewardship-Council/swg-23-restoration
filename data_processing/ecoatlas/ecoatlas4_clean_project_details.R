# This script processes all the json files to extract key information.
# For more information, see intro_to_ecoatlas.qmd in /documentation.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up ----

# Load path to cloud directory
source("admin_scripts/init_load_paths.R")

# Load functions for this scrape.
source(file.path(pth$home,
                 "data_processing", 
                 "ecoatlas", 
                 "funs_ecoatlas.R"))

# load libraries
load_libs(c("jsonlite", "tidyverse", "sf", "future", "furrr"))

# create paths 
pth$eco_data <- file.path(pth$data, 
                          "scraped_data",
                          "ecoatlas")

pth$listings <- file.path(
  pth$eco_data,
  "project_listings"
)

pth$details <- file.path(
  pth$eco_data,
  "project_details"
)

pth$json <- file.path(
  pth$details,
  "raw_json_files"
)

pth$deliverables <- file.path(
  pth$details,
  "deliverables"
)

# Load project listing
listing <- read_csv(
  file.path(
    pth$listings,
    "bay_delta_plus_project_listings.csv"
    )
)

# Load Geo Files (to get list of project ids)
geo_ <- list(
  poly = st_read(
    dsn = file.path(pth$deliverables, 
                    "ecoatlas_bay_delta_geometries.gpkg"),
    layer = "MULTIPOLYGON"
  ),
  pt = st_read(
    dsn = file.path(pth$deliverables, 
                    "ecoatlas_bay_delta_geometries.gpkg"),
    layer = "POINT"
  )
)

# Get vector of projects in our geography, then rename it to match json file.
file_ids <- c(geo_$poly$file_id, 
  geo_$pt$file_id) %>%
  unique()

file_ids <- map_vec(file_ids, function(x){
  paste0("project_",x,".json")
})

# Now, extract project info from json files.
# We'll use futureverse to do this in parallel
plan(multisession)
project_info <- future_map(file_ids, function(.file){
  # read json file.
  .json <- tryCatch({
    read_json(file.path(pth$json, .file))
  },
    error = function(cond){
      NULL
    }
  )
  
  # if null, return null
  if(is.null(.json)){
    return(NULL)
  }
  
  # return
  # we'll use the custom functions from funs_ecoatlas.R
  convert1(.json) %>%
    simplify_list() %>%
    # now just grab the tibble for the project info
    `[[`("project")
})

# turn list to tibble
project_info <- project_info %>%
  bind_rows()

# # write this to deliverables
# write_csv(project_info,
#           file.path(
#             pth$deliverables,
#             "project_info_only.csv"
#           ))



activity_type <- future_map(file_ids, function(.file){
  # read json file.
  .json <- tryCatch({
    read_json(file.path(pth$json, .file))
  },
  error = function(cond){
    NULL
  }
  )
  
  # if null, return null
  if(is.null(.json)){
    return(NULL)
  }

  # return
  # we'll use the custom functions from funs_ecoatlas.R
  list_ <- convert1(.json) %>%
    simplify_list() 
    # now just grab the tibble for the project info
  
  tryCatch({
    list_$sites$activities$activity
  },
  error = function(cond){
    NULL
  })
  
})
