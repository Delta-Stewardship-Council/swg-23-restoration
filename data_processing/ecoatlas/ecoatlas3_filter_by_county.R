# In this script we'll load each json file and determine if it is located in our general region of interest which we'll determine by counties present in our spatial extent.
# Notice, this does not do a spatial subset, this is merely a winnowing down of our json files.

# For more information, see intro_to_ecoatlas.qmd in /documentation.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up ----

# Load path to cloud directory
source("admin_scripts/init_load_paths.R")

# load libraries
load_libs(c("jsonlite", "tidyverse", "sf", "future", "furrr"))

# Load functions for this scrape.
source(file.path(pth$home,
                 "data_processing", 
                 "ecoatlas", 
                 "funs_ecoatlas.R"))

## Paths ----

# create paths for input/output
pth$census <- file.path(
  pth$data,
  "US_CENSUS"
)

# project spatial frame/boundaries, ie. Bay-Delta extent.
pth$frame <- file.path(
  pth$data,
  "bay_delta_boundary"
)

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

pth$removed_json <- file.path(
  pth$details,
  "discarded_json_files"
)

pth$unprocessed_geoms <- file.path(
  pth$details,
  "unprocessed_geoms"
)

pth$deliverables <- file.path(
  pth$details,
  "deliverables"
)

check_dir(pth$removed_json)
check_dir(pth$unprocessed_geoms)
check_dir(pth$deliverables)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make a logger ---- 
if(!exists("log_eco")){
  log_eco <- make_log(
    .filepath = file.path(pth$eco_data, "log.txt"),
    .purpose = "Record of EcoAtlas Scrape",
    .newfile = F
  )
}

## Load Project Counties ----
# (derived from tigris::counties(state = "06", year = 2021))
counties_sf <- st_read(
  dsn = file.path(
    pth$census,
    "ustiger_project_data.gpkg"
  ),
  layer = "counties"
)

# obtain vector of county names
prj_counties <- counties_sf$NAME

## Load project json file list. ----
raw_json_files <- list.files(pth$json)

# First, sorting of json files. ----
log_eco("Begin sorting json files to remove projects outside of our sample frame.")

# create log for just this process.
log_json <- make_log(
  .filepath = file.path(
    pth$details,
    "log_json.txt"
  ),
  .purpose = "Record of JSON Sorting",
  .newfile = T
)

# We will do the following use parallel processing with the futureverse.
future::plan(multisession)

# Remove unwanted json files (ie. projects outside our boundary)
future_walk(raw_json_files, function(.file){
  # read the json file.
  json1 <- read_json(
    file.path(
      pth$json,
      .file
    )
  )
  
  # get project id
  file_id <- str_extract(.file, "\\d+(?=\\.json$)")
  
  # obtain county NAMES, eg "Sacramento".
  # make sure these NAMES conform to TIGER county names.
  co_nms <- json1$counties %>%
    map(., ~{.x$name}) %>%
    unlist() %>%
    str_to_title() %>%
    str_remove(., pattern = regex("county", ignore_case = T)) %>%
    str_squish()
  
  # Check that the county is in our desired locale.
  if(!(T %in% (co_nms %in% prj_counties))){
    # log issue
    log_json(
      paste0(
        .file, 
        " counties: ", 
        paste0(co_nms, collapse = ", "),
        ". JSON file removed."
        )
      )
    
    # Copy the file over to removal dir.
    file.copy(
      from = file.path(
        pth$json,
        .file
      ),
      to = file.path(
        pth$removed_json,
        .file
      )
    )
    
    # Now actually delete the original json.
    file.remove(
      file.path(
        pth$json,
        .file
      )
    )
    
    # return early
    return()
  }  # End if statement
  
  # Now, let's extract spatial information should it exist.
  # First we'll convert the list children into data.frames.
  json2 <- convert1(json1)
  
  num_sites <- length(json2$sites)
  
  if(num_sites == 0){
    # log issue
    log_json(
      paste0(
        .file, 
        " contains no sites.", 
        " JSON file removed."
      )
    )
    
    # Copy the file over to removal dir.
    file.copy(
      from = file.path(
        pth$json,
        .file
      ),
      to = file.path(
        pth$removed_json,
        .file
      )
    )
    
    # Now actually delete the original json.
    file.remove(
      file.path(
        pth$json,
        .file
      )
    )
    
    # return early
    return()
    
    # OTHERWISE, extract sites.
  } else if (num_sites > 1) {
    sites_ <- map_dfr(json2$sites,
                      ~ {
                        .x$site %>%
                          mutate(file_id = file_id)
                      }) 
  } else {
    sites_ <- json2$sites[[1]]$site %>%
      mutate(file_id = file_id)
  }
  
  # write to file.
  write_csv(
    x = sites_,
    file = file.path(
      pth$unprocessed_geoms,
      paste0("geoms_", file_id, ".csv")
    )
  )
  
},
.progress = T)

# Second sorting ----
# The first filter removes many of the projects outside
# our scope based on the county registered in ecoatlas. 
# In other words, we have yet to do a spatial subset.
# Now let's remove the projects outside our frame by spatial subsetting.
# We do the following in this section:
# 1. Remove sites that have no spatial information. These get sorted to a
# csv file in the deliverables folder, just in case we are later made aware
# of their spatial location. Output: potential_project_sites_without_geo.csv
# 2. Transform text based information into sf objects. The output
# is the object geo_list, which contains different types of objects.
# 3. Spatial Subset. Begin by loading project geometry.


## Step 1. ----

# get all geom csv's filenames
geoms <- list.files(pth$unprocessed_geoms)

# run in parallel
future::plan(multisession)

# read csvs
geo_tb <- future_map_dfr(geoms, function(.file){
  read_csv(file = file.path(pth$unprocessed_geoms, .file)) %>%
    suppressMessages() %>%
    suppressWarnings() 
}, .progress = T)

# which sites have info and which sites don't?
geo_tb <- geo_tb %>% 
  mutate(has_something = case_when(
    !is.na(geom) ~ T,
    !is.na(latitude) & !is.na(longitude) ~ T,
    .default = F
  ))

# separate out data with and without geometries.
no_geo_tb <- geo_tb %>%
  filter(has_something == F) %>%
  select(-has_something)

write_csv(
  x = no_geo_tb,
  file = file.path(
    pth$deliverables,
    "potential_project_sites_without_geo.csv"
  )
)

# create a log file for deliverables
log_de <- make_log(.filepath = file.path(pth$deliverables, 
                                         "deliverables_log.txt"),
                   .purpose = "Deliverables Log",
                   .newfile = T)

log_de(
  "The file potential_project_sites_without_geo.csv created. These are project sites without spatial geometries defined. Confusingly, some of these sites may be part of projects that have other sites with geometries. For the sake of completeness, these files were included in the deliverables. Please note, there is no way to know with certainty whether these sites exist in our boundaries or not."
)

## Step 2. ----

# Now lets create our spatial data set.
# First, subset the data with some sort of spatial info.
geo_tb2 <- geo_tb %>%
  filter(has_something == T) %>%
  select(-has_something)

geo_list <- process_spatial_data(geo_tb2)

# We'll go ahead and remove the dropped_data element in geo_list.
# Theoretically, there should be nothing in it anyhow.

geo_list <- geo_list[names(geo_list) != "dropped_data"]

log_eco("Sorting of json files complete.")

## Step 3. ----

# load sample frame
frame_ <- st_read(file.path(pth$frame, "Bay_EcoLegalDelta.shp"))

frame_crs <- st_crs(frame_)

# Take the different spatial elements and run a spatial filter.
# In this case, the spatial filter will be a simple intersect
geo_list2 <- map(geo_list, function(obj){
  obj %>%
    st_transform(crs = frame_crs) %>%
    st_filter(y = frame_,
              .predicate = st_intersects)
})

# write these to a gpkg
walk2(geo_list2, names(geo_list2), function(obj, nm){
  st_write(
    obj = obj,
    dsn = file.path(pth$deliverables, "ecoatlas_bay_delta_geometries.gpkg"),
    layer = nm,
    # assuming that IF we re-run this code,
    # that we do actually want to overwrite.
    delete_layer = T
  )
})

log_de("A geopackage (gpkg) created, which contains all projects WITH geometries AFTER intersecting with the bay-delta boundary sample frame/extent.")

## Resolve project listing ----
# Since there were projects that were not appropriately selected for our
# project geography, lets figure out if any of the keys we used in our listing
# were wrong. Please note, this is a rough sketch. We could do more to winnow
# this down. For instance, once we've created a listing of API parameters, we 
# could then filter this list down even further by cutting out parameters that
# yield redundant project listings, but actually have little to do with our
# frame, eg parameters that group all of Northern California, including the
# Sacramento Valley. This parameter could be skipped, since its redundant.
# What this all ultimately offers us is a way to determine if certain projects
# were excluded simply because they lack spatial information on EcoAtlas.
# This could lead us to find more projects and do specific requests from
# agencies.

# first load the listing
listing <- read_csv(
  file.path(
    pth$listings,
    "bay_delta_plus_project_listings.csv"
  )
)

relevant_ids <- map(geo_list2, function(obj){
  obj %>%
    pull(file_id)
}) %>%
  unlist() %>%
  unique()

relevant_listings <- listing %>%
  filter(projectid %in% relevant_ids) %>%
  select(type, key) %>%
  distinct()

write_csv(x = relevant_listings, file = file.path(pth$deliverables, "relevant_ecoatlas_keys.csv"))

log_de("Created relevant_ecoatlas_keys.csv, which contains the EcoAtlas REST API parameters that include relevant projects to our research project. Why does this exist? Its for future reference, to whittle down the list of API request we make, which is more in service to us, than it is to EcoAtlas' limited server resources, because it reduces the number of projects we have to sift through. ALSO, and more IMPORTANTLY, it may signal projects that fall under certain API parameters that were sorted OUT of this final list because they do not contain geometries. These projects might need a follow up request.")

log_eco("ecoatlas3...R complete.")

