# Load path to cloud directory
source("admin_scripts/init_load_paths.R")

# Load libraries
load_libs(.libs = c(
  "tidyverse",
  "sf"
))

bd <- st_read(file.path(pth$data, "bay_delta_boundary", "Bay_EcoLegalDelta.shp"))
buf <- st_buffer(bd, dist = 3218.69) 

buf$name <- "2mi_buffer"

buf <- buf %>%
  select(name)

st_write(buf, 
         dsn = file.path(pth$data, 
                              "bay_delta_boundary", 
                              "buffers.gpkg"),
         layer = "2mi_buffer")
