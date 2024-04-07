# In this script, we combine all the data collected from different sources into one locale on our cloud server.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup ----
if(!exists("path_home")){
  # home directory: swg-23-restoration
  print("Variable path_home created.")
  path_home <- getwd() %>%
    stringr::str_extract(., "(.+)((swg-23-restoration(?=\\/))|(swg-23-restoration$))")
}

# load paths, crs, logger
source(file.path(path_home, 
                 "admin_scripts", 
                 "init_load_paths.R"))

# libraries
load_libs(
  c(
    "tidyverse",
    "sf"
  )
)

# other scripts
source(file.path(path_home,
                 "data_processing", 
                 "composite_data", 
                 "funs_spatial.R"))

# create file.path for new dir
pth$cdat <- file.path(pth$data, "composite_data")

# create file.path for latest step of grant program coordinates
pth$gpc <- file.path(pth$data, 
                     "grant_program_coordinates/intermediate_steps/gpc3")

# create new dir (or not if it exists)
check_dir(pth$cdat)

# Load bay-delta sample frame
sample_frame <- st_read(
  file.path(pth$data, 
            "bay_delta_boundary/Bay_EcoLegalDelta.shp")
) %>%
  st_transform(crs = crs_$crs_epsg)

# # For testing purposes only, simplify bay delta boundary
# simple_ <- sample_frame %>%
#   st_simplify(dTolerance = 1000)

# Create sample_frame with 5km buffer
buffer <- sample_frame %>%
  sf::st_buffer(dist = 5000)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. GPC ----
# Grant Program Coordinates Directory.
# Everything begins with the data supplied to us by various state agencies.
# This is because (theoretically) we will have other documents like planning
# proposals or recommendations. Generally speaking, we find that there are more
# projects listed on EcoAtlas or CNRA than we have sites with these adjacent
# documents.

# 1.A GPC Lon-Lat ----
# We want to first derive the lon-lat centroid for all projects. 
pth$gpc_sf <- file.path(pth$gpc, "gpc3_spatial.gpkg")
lyrs <- st_layers(pth$gpc_sf)

coords_ <- purrr::map_dfr(.x = lyrs$name,
           .f = function(nm){
             # load spatial
             sp_ <- sf::st_read(
               dsn = pth$gpc_sf,
               layer = nm
             ) %>%
               # transformation just in case
               # so it matches `buffer` crs
               sf::st_transform(crs = st_crs(buffer))
             
             # filter out non-intersecting projects
             sp_ <- sp_ %>%
               sf::st_filter(y = buffer, 
                             .predicate = sf::st_intersects)
             
             # convert sf object to tibble with lon/lat (crs 4326)
             # custom function from funs_spatial.R
             obj <- to_coords(sp_)
             
             # return
             obj
           })

# 1.B GPC Attributes ----
# create tibble of stripped down information.
gpc_atts <- purrr::map_dfr(
  .x = list.files(pth$gpc, pattern = "atts\\."),
  .f = function(file_){
    # read attributes csv
    atts <- readr::read_csv(
      file = file.path(pth$gpc, file_),
      col_types = readr::cols(.default = "c")
    )
    
    # obtain agency name from filename
    nm <- sub(pattern = "\\_.+", 
              replacement = "", 
              x = file_)
    
    # create output tibble
    out <- tibble::tibble(
      agency = nm,
      # this is the spatial object id
      nceas_obj_id = atts$nceas_id,
      filepath_gpc_atts = file.path(
        "data/grant_program_coordinates/intermediate_steps/", 
        file_),
      filepath_gpc_spatial =
        "data/grant_program_coordinates/intermediate_steps/gpc3_spatial.gpkg"
    )
    
    if(nm == "cdfw"){
      out$internal_id_name <- "Number"
      out$internal_id <- atts$Number
      # create project ID
      out$nceas_proj <- atts %>%
        group_by(Number) %>%
        mutate(id = paste0("proj_", nm, "_", cur_group_id())) %>%
        ungroup() %>%
        pull(id)
      out$project_title <- atts$Title
      
    } else if (nm == "cnra"){
      out$internal_id_name <- NA_character_
      out$internal_id <- NA_character_
      # create project ID
      out$nceas_proj <- atts %>%
        group_by("Project Name") %>%
        mutate(id = paste0("proj_", nm, "_", cur_group_id())) %>%
        ungroup() %>%
        pull(id)
      
      out$project_title <- atts$`Project Name`
      
    } else if(nm == "cscc"){
      out$internal_id_name <- "Project_Number"
      out$internal_id <- atts$Project_Number
      # create project ID
      out$nceas_proj <- atts %>%
        group_by(Project_Number) %>%
        mutate(id = paste0("proj_", nm, "_", cur_group_id())) %>%
        ungroup() %>%
        pull(id)
      out$project_title <- atts$Project_Name
      
    } else if(nm == "sfbra"){
      out$internal_id_name <- "projectid"
      out$internal_id <- atts$projectid
      # create project ID
      out$nceas_proj <- atts %>%
        group_by(projectid) %>%
        mutate(id = paste0("proj_", nm, "_", cur_group_id())) %>%
        ungroup() %>%
        pull(id)
      out$project_title <- atts$projectname
      
    } else if(nm == "ssjdc"){
      out$internal_id_name <- "ProjectID"
      out$internal_id <- atts$ProjectID
      # create project ID
      out$nceas_proj <- atts %>%
        group_by(ProjectID) %>%
        mutate(id = paste0("proj_", nm, "_", cur_group_id())) %>%
        ungroup() %>%
        pull(id)
      out$project_title <- atts$ProjectName
    }
    
    # add coords_ back in
    out <- out %>%
      left_join(y = coords_, 
                by = c("nceas_obj_id" = "nceas_id"))
    
    # return
    out %>%
      select(nceas_proj, 
             nceas_obj_id, 
             agency, 
             lon, 
             lat, 
             project_title,
             everything())
  })

write_csv(
  x = gpc_atts,
  file = file.path(
    pth$cdat,
    "step1.csv"
  )
)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Ecoatlas ---
# 
# ecoa <- read_csv(
#   file = file.path(
#     pth$data, 
#     "scraped_data/ecoatlas/project_details/deliverables/project_info_only.csv"),
#   col_types = readr::cols(.default = "c")
# )
# 
# ecoa2 <- ecoa %>%
#   select(projectname, projectid) %>%
#   rename(ecoatlas_projectid = projectid)
# 
# gpc_atts2 <- gpc_atts %>%
#   left_join(y = ecoa2, by = c("project_title" = "projectname"))
# 
# gpc_atts3 <- gpc_atts2 %>%
#   mutate(best_ecoatlas_match = case_when(
#     is.na(ecoatlas_projectid) ~ match_ecoatlas(
#       txt = project_title,
#       ecoatlas = ecoa2),
#     .default = NA
#   ))
