# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load packages
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Loads libraries from a vector of library names, on the condition they don't 
#' exist in the environment already.
#' 
#' @param .libs character vector
#' @return NULL
load_libs <- function(.libs){
  lapply(
    X = .libs,
    FUN = function(lib){
      if(!(lib %in% .packages()))
        library(lib, character.only = T) |>
        suppressMessages() 
    }
  ) |>
    invisible()
}

load_libs(c("stringr", "jsonlite"))

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function definition: check_dir
#' Takes a path to file and checks if the directory exists. If not, it creates
#' the directory. Either way, it informs you of its progress.
#' 
#' @param .path character, the file path.
#' @param .msg logical, whether to deliver a message or not.
#' @return NULL
check_dir <- function(.path, 
                      .msg = T){
  if(!dir.exists(.path)){
    msg <- paste0(
      "Creating path to: ",
      .path,
      ".\n"
    )
    
    dir.create(.path)
  } else {
    msg <- paste0(
      "Path, ",
      .path,
      ", exists.\n"
    )
  }
  if(.msg){
    message(msg)
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check for a directory, if not make it.
check_dir(path_data)