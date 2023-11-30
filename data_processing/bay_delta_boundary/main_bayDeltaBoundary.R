

# TEMPORARY
# CNRA scrape branch needs to be merged. Once this is done, 
# remove this lapply:
lapply(
  X = c(
    "stringr",
    "jsonlite",
    "purrr"
  ),
  FUN = function(x) {
    if(!(x %in% .packages())){
      print(paste0("Loading {", x, "}"))
      library(x, character.only = T)
    }
  }
) |>
  invisible()

# Load initial paths ----
source("admin_scripts/init_load_paths.R")

if(!exists("path_bay_delta_boundary")){
  # home directory: swg-23-restoration
  print("Variable path_bay_delta_boundary created.")
  path_bay_delta_boundary <- file.path(path_data, "bay_delta_boundary")
}

if(!dir.exists(path_bay_delta_boundary)){
  print("Directory path_bay_delta_boundary created.")
  dir.create(path_bay_delta_boundary)
}

# Required Packages -------------------------------------------------------
lapply(
  X = c(
    "sf",
    "nngeo",
    "foreach",
    "dplyr",
    "tools",
    "urltools"
  ),
  FUN = function(x) {
    if(!(x %in% .packages())){
      print(paste0("Loading {", x, "}"))
      library(x, character.only = T)
    }
  }
) |>
  invisible()

# Bespoke function
source("data_processing/bay_delta_boundary/funs_BayEcoLegalDelta_shpBoundary.R")

# Run function -----------------------------------------------
test <- create_BayDelta_shp(path_bay_delta_boundary)
plot(st_geometry(test))