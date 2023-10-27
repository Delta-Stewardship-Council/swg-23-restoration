# packages
if(!("stringr" %in% .packages())){
  library(stringr)
}

if(!("jsonlite" %in% .packages())){
  library(jsonlite)
}

# Creates file.paths.
# home directory: swg-23-restoration
path_to_home <- getwd() %>%
  str_extract(., "(.+)((swg-23-restoration(?=\\/))|(swg-23-restoration$))")

path_to_data <- jsonlite::read_json("paths.json")$box_path

