# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load initial paths ----
source("admin_scripts/init_load_paths.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load libraries ----
lapply(
  X = c(
    "tidyverse",
    "polite",
    "sf"
  ),
  FUN = function(x) {
    if(!(x %in% .packages())){
      print(paste0("Loading {", x, "}"))
      library(x, character.only = T)
    }
  }
) |>
  invisible()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create more paths. ----
# branch folder
path_branch <- file.path(path_data, "scraped_data", "cnra")

if(!dir.exists(path_branch)){
  print("Creating branch dir.")
  dir.create(path_branch)
}

# raw html dir
path_raw <- file.path(path_branch, "raw_html")

if(!dir.exists(path_raw)){
  print("Creating raw html dir.")
  dir.create(path_raw)
}

# programs dir
path_prog <- file.path(path_raw, "programs")

if(!dir.exists(path_prog)){
  print("Creating prop1 programs dir.")
  dir.create(path_prog)
}

# projects dir
path_proj <- file.path(path_raw, "projects")

if(!dir.exists(path_proj)){
  print("Creating prop1 projects dir.")
  dir.create(path_proj)
}

# program json directory
path_progjson <- file.path(path_data, 
                           "scraped_data", 
                           "cnra", 
                           "raw_html", 
                           "program_json")

if(!dir.exists(path_progjson)){
  print("Creating program_json dir.")
  dir.create(path_progjson)
}

# project json directory
path_projjson <- file.path(path_data, 
                           "scraped_data", 
                           "cnra", 
                           "raw_html", 
                           "project_json")

if(!dir.exists(path_projjson)){
  print("Creating project_json dir.")
  dir.create(path_projjson)
}

# project spatial directory
path_spatial <- file.path(path_data, 
                           "scraped_data", 
                           "cnra", 
                           "raw_html", 
                           "project_spatial")

if(!dir.exists(path_spatial)){
  print("Creating path_spatial dir.")
  dir.create(path_spatial)
}

# project spatial directory
path_clean <- file.path(path_data, 
                          "scraped_data", 
                          "cnra", 
                          "clean_data")

if(!dir.exists(path_clean)){
  print("Creating path_clean dir.")
  dir.create(path_clean)
}

print("Path variables specific to CNRA scraping created.")
# End of Setup
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~