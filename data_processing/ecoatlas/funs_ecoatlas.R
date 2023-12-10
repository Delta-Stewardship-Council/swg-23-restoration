
#' Supply a single regiontype and key and retrieve a list of projects.
#' 
#' @param .type character, is the region type key, including adminregion, 
#' ecoregion, group, waterboard.
#' @param .key character, is the key name, eg. bay-delta.
#' @param .agent character, please enter your name or your org's name.
get_proj_list <- function(
    .type, 
    .key,
    .agent = "NA"
    ){
  load_libs(c("httr2", "tidyverse"))
  
  if(.type %in% c("adminregion", "ecoregion", "waterboard")){
    req_str <- "api.ecoatlas.org/projects/{.type}/{.key}" %>%
      str_glue()
  } else if(.type == "group"){
    req_str <- "api.ecoatlas.org/projects/group/?q={.key}" %>%
      str_glue()
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Build request.
  # (This only designs our request.)
  req <- req_str %>%
    # design request
    httr2::request(.) %>%
    # Add your identifier in case your code causes problems
    httr2::req_user_agent(.agent) %>%
    # set max number of attempts in case it fails
    httr2::req_retry(max_tries = 3)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Run the GET command.
  # (This actually connects to the website.)
  resp <- httr2::req_perform(req,
                      # verbosity simply gives us updates as it goes.
                      verbosity = 0)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract JSON object (as a list)
  # (This only derives information stored in `resp`.)
  resp_json <- resp %>%
    resp_body_json()
  
  # create a tibble that stores all the relevant project info.
  projs <- resp_json$projects %>%
    map_dfr(., as_tibble) %>%
    mutate(
      type = .type,
      key = .key
    ) %>%
    select(type, key, everything())
  
  # return
  projs
}

#' Supply a list of regions, groups, etc, and retrieve a list of projects from
#' the EcoAtlas.
#' 
#' @param .tb tibble, with two columns, TypeKey and key
#' @param .agent character, your name or org's name.
#' @return a tibble with five columns, including input columns,
#' projectid, projectname, and number of duplicates present in this tibble.
get_listings <- function(
    .tb,
    .agent = "NA"
){
  load_libs(c("tidyverse"))
  
  all_proj <- map2_dfr(.x = .tb$TypeKey, 
                       .y = .tb$key, 
                       .f = ~get_proj_list(
                         .type = .x, 
                         .key = .y,
                         .agent = .agent),
                       .progress = T)
  
  message("All EcoAtlas project lists completed.")
  
  all_proj <- all_proj %>%
    group_by(projectid) %>%
    mutate(num_dupes = n()) %>%
    ungroup()
  
  # return
  all_proj
}

#' Supply project IDs and download each one. Note, this is not optimized for parallel processing because we assume polite-only REST.
#' 
#' @param .ids numeric, vector of project ID numbers.
#' @param .dir character, output directory.
#' @param .agent character, your name or org's name.
get_projs <- function(
    .ids,
    .dir,
    .agent = "NA"
){
  # load libraries
  load_libs(c("jsonlite", "tidyverse", "httr2"))
  
  # double-check all ids are unique
  .ids <- unique(.ids)
  
  # get file list
  dir_ls <- list.files(path = .dir)
  
  # process requests and return json
  jsons <- map(.ids, function(id){
  
    # what will the filename be?
    nm <- paste0("project_", id, ".json")
    
    # does it already exist?
    if(nm %in% dir_ls){
      return(NULL)
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Build request.
    # (This only designs our request.)
    req <- str_glue(
      "api.ecoatlas.org/project/{id}"
    ) %>%
      # design request
      httr2::request(.) %>%
      # Add your identifier in case your code causes problems
      httr2::req_user_agent("NCEAS Restoration Group") %>%
      # set max number of attempts in case it fails
      httr2::req_retry(max_tries = 3)
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Run the GET command.
    # (This actually connects to the website.)
    resp <- httr2::req_perform(req,
                        # verbosity simply gives us updates as it goes.
                        verbosity = 0)
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Extract JSON object (as a list)
    # (This only derives information stored in `resp`.)
    resp_json <- resp %>%
      resp_body_json()
    
    # define output file path
    pth <- file.path(.dir, nm)
    
    # write to cloud
    write_json(
      x = resp_json,
      path = pth
    )
    
    # return
    resp_json
  },
  .progress = T)
  
  message("Scraping project details complete.")
  
  # return
  jsons
}