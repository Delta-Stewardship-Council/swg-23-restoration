
# Download Different Listings of Projects ----

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

# Download Project Details ----

#' Supply project IDs and download each one. Note, this is not optimized for parallel processing because we assume polite-only REST.
#' 
#' @param .ids numeric, vector of project ID numbers.
#' @param .dir character, output directory.
#' @param .agent character, your name or org's name.
#' @param .return logical, if true, will return a (large) list.
#' @return either NULL or a large list.
get_projs <- function(
    .ids,
    .dir,
    .agent = "NA",
    .return = F
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
    if(.return == T){
      resp_json
    } else {
      NULL
    }
  },
  .progress = T)
  
  message("Scraping project details complete.")
  
  # return
  if(.return == T){
    jsons
  } else {
    NULL
  }
}

# Details Cleaning Functions ----

#' Converts a list into a tibble. This only works if there are no sublist elements.
#' @param list_ is a list object which contains only vectorizable elements.
convert_layer_to_tb <- function(list_){
  tb <- map2_dfc(list_, names(list_), function(x, nm){
    y <- unlist(x)
    if(is.null(y)){
      y <- as.character(NA)
    }
    tibble(
      !!sym(nm) := y %>% as.character()
    )
  })
  # return
  tb
}

#' Takes a list and assess whether it has sub lists or not.
#' @param list_ is a list object.
#' @return a tibble or a single value (NA or NULL). A tibble will include information on whether there is content in the supplied list.
assess <- function(list_){
  # If this isn't a list, or it has no elements, return NULL
  if(
    !(class(list_) %in% "list") |
    length(list_) == 0
  ){
    return(NULL)
  }
  
  # If there are no names in this list, return NA
  if(is.null(names(list_))){
    return(NA)
  }
  
  map2_dfr(
    .x = list_, 
    .y = names(list_), 
    .f = function(x, nm){
      # Is this element a list object?
      islist <- is.list(x)
      
      # Create default values for output
      element_len <- as.numeric(NA)
      has.sub.list <- as.logical(NA)
      
      # IF-1
      if(islist){
        # Is a list so,
        # get length
        element_len <- length(x)
        
        # IF-1.1
        if(element_len > 0){
          # Has content so,
          yvec <- map_vec(x, function(y){
            islisty <- is.list(y)
          })
          
          # Does it have sub lists?
          has.sub.list <- T %in% yvec
          
        }  # End of IF-1.1
      } # End of IF-1
      
      # Convert this into a tibble
      tb <- tibble(
        nm = nm,
        islist = islist,
        element_len = element_len,
        has.sub.list = has.sub.list
      )
      
      # Return
      tb
    })  # end map assessment
}

#' Converts a list object into a list of tibbles.
#' @param list_ is the main list object that has several dependent lists.
#' @return a list object of tibbles.
convert1 <- function(list_){
  # Assess if this list has other sublists present.
  assessment <- assess(list_)
  
  if(length(assessment) == 1){
    if(is.null(assessment)){
      stop("Object supplied is not a list!")
    }
    
    # If its NA, then we know this list has only sublists
    if(is.na(assessment)){
      return(
        map(.x = list_, ~convert1(.x))
      )
    }
  }
  
  # Check to see if there are sub lists present
  sub.lists <- assessment %>%
    filter(has.sub.list == T)
  
  if(nrow(sub.lists) != 0){
    # run this function over each element of this new list of sublists.
    out <- map2(.x = list_[sub.lists$nm], 
                .y = sub.lists$nm,
                .f = function(x, nm){
                  convert1(x)
                }) %>%
      set_names(sub.lists$nm)
    
    # Turn other elements into a tibble.
    other_elements <- assessment %>%
      filter(is.na(has.sub.list) | has.sub.list == F)
    
    if(nrow(other_elements) > 0){
      other_elements <- list_[other_elements$nm]
      
      other_elements <- convert_layer_to_tb(other_elements)
      
      out <- append(out, list(other = other_elements))
    }
    
  } else {
    out <- convert_layer_to_tb(list_)
  }
  
  # Return
  out
}

#' Converts the complicated output of `convert1` and drops empty levels between nested lists, as well as binding rows of common tibbles.
#' @param list_ is the output of `convert1`
#' @param root_nm is the previous list's name.
#' @return a list object (which may still contain nested lists).
simplify_list <- function(list_, root_nm = "root"){
  len <- length(list_)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # IF-1~~~~IF-1~~~~IF-1~~~~START
  if(len > 0){
    # Get names of elements, and get class of elements
    nms <- names(list_)
    cls <- map_vec(list_, ~class(.x)[1])
    
    # First, lets deal with unnamed (and named) elements
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # IF-1.1~~~~IF-1.1~~~~IF-1.1~~~~START
    if(len == 1 & 
       cls[1] == "list"){
      # 1.1Case: Special
      # One, unnamed list present.
      # Solution, skip this level.
      return(
        simplify_list(
          list_ = unlist(list_, 
                         recursive = F),
          root_nm = root_nm)
      )
      
    } else if(is.null(nms)){
      # 1.1Case2: 
      # This means there are no names,
      # and there are >1 elements.
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # IF-1.1.1~~~~IF-1.1.1~~~~IF-1.1.1~~~~START
      if(all(cls == "list")){
        # This means, 
        # there are no names,
        # there are >1 elements,
        # and all elements are lists.
        list_ <- list_ %>%
          set_names(paste0(root_nm, ".", 1:len))
        
      } else {
        # This means
        # there are no names,
        # there are >1 elements,
        # and at least 1 element is not a list.
        new_nms <- map2_vec(
          cls, 
          seq_along(cls), 
          function(cl, idx){
            if(cl == "list"){
              paste0(root_nm, ".", idx)
            } else if (cl == "tbl_df"){
              paste0("tb.", idx)
            } else {
              paste0(cl, ".", idx)
            }
          })
        
        # rename
        list_ <- list_ %>%
          set_names(new_nms)
        
      }  # END IF-1.1.1
      # IF-1.1.1~~~~IF-1.1.1~~~~IF-1.1.1~~~~END
      
    } else if("" %in% nms){
      # 1.1Case3: 
      # This means there is at least one object with a name,
      # and at least one object without a name.
      loc <- which(nms == "")
      # create names for each item with the root name 
      # and the which location.
      new_nms <- map2_vec(
        cls[loc], 
        loc, 
        function(cl, idx){
          if(cl == "list"){
            paste0(root_nm, ".", idx)
          } else if (cl == "tbl_df"){
            paste0("tb.", idx)
          } else {
            paste0(cl, ".", idx)
          }
        })
      
      # apply the new names
      names(list_)[loc] <- new_nms
      
    }  # END IF-1.1
    # IF-1.1~~~~IF-1.1~~~~IF-1.1~~~~END
    
    # Next, we'll look at if the elements are:
    # a. all lists
    # b. all tibbles/other-non-list-objs
    # c. a mix of a and b.
    # And then apply simplify_list to the elements
    # that are themselves lists.
    
    # IF-1.2~~~~IF-1.2~~~~IF-1.2~~~~START
    if(all(cls == "list")){
      # a. all lists
      list_ <- map(list_, simplify_list)
      
    } else if(all(cls != "list")){
      # b. all tibbles/other-non-list-objs
      
      if(all(cls == "tbl_df")){
        # They're all tibbles
        # Do they appear to have the same headings?
        headings <- map_vec(list_, 
                            ~paste0(names(.x), 
                                    collapse = ",")) %>% 
          unique()
        if(length(headings) == 1){
          # They have the same headings so bindrows and return.
          return(
            bind_rows(list_)
          )
        }
      }
      
      
    } else {
      # c. a mix of a and b.
      list_ <- map2(.x = cls, 
                    .y = seq_along(cls), 
                    .f = function(cl, idx){
                      if(cl == "list"){
                        simplify_list(
                          list_[[idx]],
                          names(list_[idx])
                        )
                      } else {
                        list_[[idx]]
                      }
                    })
    }
    # IF-1.2~~~~IF-1.2~~~~IF-1.2~~~~END
    
    return(list_)
  } else {
    # If length is 0
    return(NULL)
  } # END IF-1
  # IF-1~~~~IF-1~~~~IF-1~~~~END
}  # end of function

#' Write a list of tibbles and other lists of tibbles to dir.
#' @param list_ is the output of `simplify_list`.
#' @param dir is the output of `tempdir()`.
#' @return a vector of file paths to files themselves.
write_to_tempdir <- function(list_, dir){
  map2(
    .x = list_, 
    .y = names(list_), 
    .f = function(x, nm){
      
      if("list" %in% class(x)){
        out_path <- file.path(dir, nm)
        dir.create(out_path)
        out_file <- write_to_tempdir(x, out_path)
      } else if("tbl" %in% class(x)){
        out_file <- file.path(dir, 
                              paste0(nm, ".csv")
        )
        write_csv(
          x = x,
          file = out_file
        )
      } else {
        warning(str_glue("An item, {nm}, was not written to dir."))
      }
      # Return
      out_file
    })  # End of walk2
}


# Spatial ----

#' @param data is a tibble of unprocess data with spatial geometries present either as a WKT column (geom) or lat/lon columns.
process_spatial_data <- function(data) {
  # Split the data
  data_with_geom <- data %>% 
    filter(!is.na(geom))
  data_with_latlon <- data %>% 
    filter(is.na(geom) & !is.na(latitude) & !is.na(longitude))
  data_to_drop <- data %>% 
    filter(is.na(geom) & (is.na(latitude) | is.na(longitude)))
  
  # Set a common CRS target
  crs_target <- 4326 # WGS84 CRS
  
  # Process data with geom
  sf_data_with_geom <- st_as_sf(data_with_geom, wkt = "geom")
  sf_data_with_geom <- st_transform(sf_data_with_geom, crs_target)
  sf_data_with_geom <- sf_data_with_geom %>% select(-geom) # Drop the original geom column
  names(sf_data_with_geom)[which(names(sf_data_with_geom) == "geometry")] <- "geometry" # Rename geometry column if needed
  
  # Split sf_data_with_geom by geometry type
  geom_types <- unique(st_geometry_type(sf_data_with_geom))
  sf_data_by_geom_type <- lapply(geom_types, function(geom_type) {
    sf_data_with_geom[st_geometry_type(sf_data_with_geom) == geom_type, ]
  })
  names(sf_data_by_geom_type) <- geom_types
  
  # Process data with lat/lon
  points_list <- pmap(data_with_latlon[c("longitude", "latitude")], function(longitude, latitude) {
    st_point(c(longitude, latitude), dim = "XY")
  })
  sfc_points <- do.call(st_sfc, points_list)
  st_crs(sfc_points) <- crs_target
  sf_points <- st_sf(data_with_latlon, geometry = sfc_points)
  sf_points <- sf_points %>% select(-latitude, -longitude, -datum) # Drop columns used for creation
  sf_data_by_geom_type[["POINT"]] <- sf_points
  
  # Standardize column names and drop unnecessary columns
  sf_data_by_geom_type <- lapply(sf_data_by_geom_type, function(sf_object) {
    if ("geometry" %in% names(sf_object)) {
      sf_object
    } else {
      sf_object %>% rename(geometry = geom)
    }
  })
  
  # Output list containing sf objects by geometry type and dropped data
  output <- c(sf_data_by_geom_type, list(dropped_data = data_to_drop %>% select(-latitude, -longitude, -datum, -geom)))
  
  return(output)
}