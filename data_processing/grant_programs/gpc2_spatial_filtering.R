# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up ----

# Load path to cloud directory
source("admin_scripts/init_load_paths.R")

library(tidyverse)
library(readxl)
library(sf)

# create path to grant program coordinates directory
pth$gpc <- file.path(pth$data, "grant_program_coordinates")

# raw data
pth$raw <- file.path(pth$gpc, "raw")

# create output path
pth$out <- file.path(pth$gpc, "deliverables")

pth$docs <- file.path(pth$out, "documentation.csv")

# create path to geopackage
pth$gpkg <- file.path(pth$out, "grant_programs_spatial.gpkg")

# create path to project boundaries.
pth$bound <- file.path(pth$data, "bay_delta_boundary", "Bay_EcoLegalDelta.shp")

# get our project boundary
bound <- st_read(pth$bound)

# generate table of geopackage items inside the bound
out <- map_dfr(st_layers(pth$gpkg)$name, function(x){
  obj <- st_read(
    dsn = pth$gpkg,
    layer = x
  )
  
  inside <- obj %>%
    st_filter(bound) %>%
    st_set_geometry(NULL) %>%
    mutate(inside_bay_delta_boundary = T)
  
  obj <- obj %>%
    st_set_geometry(NULL) %>%
    left_join(inside, by = c("file_id", "obj_id")) %>%
    mutate(inside_bay_delta_boundary = ifelse(
      is.na(inside_bay_delta_boundary), 
      F, 
      inside_bay_delta_boundary
    )
    )
  
  # return
  obj %>%
    mutate(layer_name = x) %>%
    arrange(inside_bay_delta_boundary) %>%
    select(layer_name, file_id, obj_id, inside_bay_delta_boundary)
})

write_csv(
  x = out,
  file = file.path(pth$out, "spatial_filter_output.csv")
)

total_inside <- out %>% filter(inside_bay_delta_boundary == T) %>% nrow()
total_outside <- out %>% filter(inside_bay_delta_boundary != T) %>% nrow()

docs <- tibble(
  filename = "spatial_filter_output.csv",
  description = str_glue(
    "Table that indicates if objects in grant_programs_spatial.gpkg can be found inside the bay-delta boundary. At the time of this writing {total_inside} objects are inside the bay-delta boundary, and {total_outside} are outside."
    ),
  date_created = format(Sys.Date()),
  source_code = "spatial_filtering.R"
)

write_csv(
  x = docs,
  file = pth$docs,
  append = T
)

write(
  x = "Spatial filtering completed.",
  file = file.path(pth$out, "log.txt"),
  append = T
)
