# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load packages
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
walk(
  .x = c(
    "stringr",
    "jsonlite"
  ),
  .f = function(x) {
    if(!(x %in% .packages())){
      print(paste0("Loading {", x, "}"))
      library(x)
    }
  }
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creates file.paths.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!exists("path_home")){
  # home directory: swg-23-restoration
  print("Variable path_home created.")
  path_home <- getwd() %>%
    str_extract(., "(.+)((swg-23-restoration(?=\\/))|(swg-23-restoration$))")
}

if(!exists("path_data")){
  # Path to cloud data
  print("Variable path_data created.")
  path_data <- jsonlite::read_json("paths.json")$box_path
}
