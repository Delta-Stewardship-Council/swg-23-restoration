# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package Loading Function ----
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

# Now load these libraries.
load_libs(c("stringr", "jsonlite"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creates File.paths ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This is the old method, left in here until all code updated.
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

# New method
if(!exists("pth")){
  pth <- list()
  
  # home directory: swg-23-restoration
  pth$home <- getwd() %>%
    str_extract(., "(.+)((swg-23-restoration(?=\\/))|(swg-23-restoration$))")
  
  pth$data <- jsonlite::read_json("paths.json")$box_path
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Directory Making Function check_dir ----
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
      "Creating directory for: ",
      .path,
      ".\n"
    )
    
    dir.create(.path, recursive = T)
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
check_dir(pth$data)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load crs ----
# Load NCEAS Restoration Group's Project Coordinate Reference System
# This is the CRS used for all spatial geometries in this project.
crs_ <- read_json("admin_scripts/project_crs.json", simplifyVector = T)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Log File Function ----
#' This is a function that creates a function to write a log. Namely, it creates a function that writes a simple text log that allows you to record events in a script.
#' 
#' @param .filepath character, this is your full path to the file, eg 
#' /USERNAME/.../log.txt on UNIX machines.
#' @param .purpose character, this serves as the header at the top of the log.
#' @param .newfile logical, should we create a new file, or assume one exists?
#' @return a function
#' @examples
#' scraping_logger <- make_log(.filepath = "swg-23-restoration/sandbox/log.txt")
make_log <- function(
    .filepath,
    .purpose,
    .newfile = T
){
  # create a log file.
  if(.newfile){
    write(
      x = paste0(
        "Log File: ",
        .purpose,
        "\nCreated on ",
        format(Sys.time(), "%Y-%m-%d %H:%M"),
        "\n\n"
      ),
      file = .filepath
    )
  }
  
  #' This function manually writes a log to file.
  #' 
  #' @param .msg character, the message.
  function(
    .msg
  ){
    if(length(.msg) > 1){
      .msg <- paste0(.msg, collapse = " ")
    }
    
    write(
      x = paste0(
        "\n[",
        format(Sys.time(), "%Y-%m-%d %H:%M"),
        "]: ",
        .msg
      ),
      file = .filepath,
      append = T
    )
  }
}
