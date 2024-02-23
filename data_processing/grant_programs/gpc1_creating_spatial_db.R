# The purpose of this script is to load all attribute tables and spatial data;
# determine if any of the spatial information should be in the attribute table
# then join it;
# remove attributes for objects that have no spatial information;
# to combine all spatial information into a single geopackage.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up ----
source(file.path("data_processing", 
                 "grant_programs", 
                 "init_gpc.R"))

# Create a logging function bespoke to this process.
logger <- make_log(
  .filepath = file.path(pth$gpc1, "log.txt"),
  .purpose = "spatial db synthesis"
)

# Reading Raw Files ----

# Because each file in the directory has different ways to organize the spatial
# information, our first task is to read each item appropriately.

## A. cnra ----
cnra1 <- "CNRA_Project_Coordinates.xlsx"
cnra_tb <- readxl::read_xlsx(
  file.path(pth$raw, "cnra", cnra1)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# create ID
tmp <- cnra_tb %>%
  mutate(nceas_id = paste0("cnra" ,row_number()))

cnra_atts <- tmp %>%
  st_set_geometry(NULL)

cnra_sf <- tmp %>%
  select(nceas_id) %>%
  st_transform(crs = crs_$crs_epsg)

rm(tmp)

### cnra write ----
write_csv(x = cnra_atts,
          file = file.path(pth$gpc1, "cnra_atts.csv"))

st_write(obj = cnra_sf,
         dsn = pth$gpc1_gpkg,
         layer = "cnra",
         delete_layer = T)

## B. sfbra ----

# This is the primary attributes table.
# We could have spatialized the lon/lat columns, but
# since these projects also appear to have shape files,
# we leave those columns alone.
sfbra1 <- "ecoatlas_sfbrafunded_projectsandsites.csv"
sfbra_tb <- readr::read_csv(
  file = file.path(pth$raw, "sfbra", sfbra1),
  show_col_types = F
) 
  # st_as_sf(coords = c("longitude", "latitude"), 
  #          crs = "WGS84",
  #          # There are projects without coords. Keep these!
  #          na.fail = F)

# This contains the geometry, 
# but confusingly it also has an attributes table.
sfbra2 <- "ecoatlas_sfbrafunded_shapefile"
sfbra_sf <- st_read(
  dsn = file.path(pth$raw, 
                  "sfbra",
                  sfbra2)
)

# Notes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# What these seem to have in common are:
# 1. projectid
# 2. siteid
# Since the main attributes table has more detailed information,
# we will join the attributes of the shp to the main attributes in the csv.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sfbra_sf <- sfbra_sf %>%
  mutate(nceas_id = paste0("sfbra" ,row_number()))

tmp <- sfbra_sf %>%
  st_set_geometry(NULL)

# final outputs for sbra
sfbra_atts <- sfbra_tb %>%
  left_join(tmp, by = c("projectid", "siteid"))

sfbra_sf <- sfbra_sf %>%
  select(nceas_id) %>%
  st_transform(crs = crs_$crs_epsg)

rm(tmp)

### sfbra write ----
write_csv(x = sfbra_atts,
          file = file.path(pth$gpc1, "sfbra_atts.csv"))

st_write(obj = sfbra_sf,
         dsn = pth$gpc1_gpkg,
         layer = "sfbra",
         delete_layer = T)

## C. cscc ----
cscc1 <- "Prop 1 Projects_SCC.xls"
cscc_tb <- readxl::read_xls(
  path = file.path(pth$raw, "cscc", cscc1) 
) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

cscc_tb <- cscc_tb %>%
  mutate(nceas_id = paste0("cscc" ,row_number()))

# final outputs
cscc_atts <- cscc_tb %>%
  st_set_geometry(NULL)

cscc_sf <- cscc_tb %>%
  select(nceas_id) %>%
  st_transform(crs = crs_$crs_epsg)

### cscc write ----
write_csv(x = cscc_atts,
          file = file.path(pth$gpc1, "cscc_atts.csv"))

st_write(obj = cscc_sf,
         dsn = pth$gpc1_gpkg,
         layer = "cscc",
         delete_layer = T)

## D. cdfw ----
cdfw1 <- "CDFW_Prop_1_68_GHG_Rest_Grants_Delta_data_20231113-no science.xlsx"
cdfw_tb <- readxl::read_xlsx(
  path = file.path(
    pth$raw, 
    "cdfw",
    cdfw1
  )
) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

cdfw_tb <- cdfw_tb %>%
  mutate(nceas_id = paste0("cdfw" ,row_number()))

cdfw_atts <- cdfw_tb %>%
  st_set_geometry(NULL)

cdfw_sf <- cdfw_tb %>%
  select(nceas_id) %>%
  st_transform(crs = crs_$crs_epsg)

### cdfw write ----
write_csv(x = cdfw_atts,
          file = file.path(pth$gpc1, "cdfw_atts.csv"))

st_write(obj = cdfw_sf,
         dsn = pth$gpc1_gpkg,
         layer = "cdfw",
         delete_layer = T)

## E. ssjdc ----

ssjdc1 <- "ECP_ProjectLocations_Points_PRIMARY.xlsx"
ssjdc_tb<- readxl::read_xlsx(
    path = file.path(
      pth$raw, 
      "ssjdc",
      ssjdc1
    )
  )

# Second directory of SSJDC is large and complicated.
# Here are the rules I will use for loading this data
# I will be agnostic and load ALL of the spatial data.
# However, I will add a new column that measures overall size
# which we can use as a filter.

# first grab a file list of all spatial files.
ssjdc2 <- "ECP_ProjectLocations_spatial"

spatial_ <- list.files(file.path(pth$raw, "ssjdc", ssjdc2),
           pattern = "\\.(shp|gdb|kmz)$",
           recursive = T,
           include.dirs = T,
           full.names = T)

# convert this list to a tibble
# Here, I'm trying to pull information in order to match projects to the ECP 
# excel sheet.
fl <- tibble(
  path = spatial_
) %>%
  mutate(dir = str_remove(path, file.path(pth$raw, "ssjdc", ssjdc2))) %>%
  mutate(base = basename(spatial_)) %>%
  rowwise() %>%
  mutate(fund_proj = str_split_1(dir, '/')[2] %>%
           str_remove(., "\\.[[:alpha:]]{3,4}$")
         ) %>%
  ungroup() %>%
  mutate(ProjectID = case_when(
    str_detect(fund_proj, "^CAR") ~ fund_proj,
    str_detect(fund_proj, "^NBS") ~ fund_proj,
    str_detect(fund_proj, "^Prop1") ~ str_extract(fund_proj, "(?<=\\_)\\d{4}$"),
    .default = fund_proj
  )) %>%
  mutate(FundingSource = case_when(
    str_detect(fund_proj, "^CAR") ~ "CAR",
    str_detect(fund_proj, "^NBS") ~ "NBS",
    str_detect(fund_proj, "^Prop1") ~ "Prop1",
    .default = fund_proj
  )) %>%
  select(ProjectID, FundingSource, everything())

if(!file.exists(file.path(pth$gpc1, "ssjdc_spatial_criteria.csv"))){
  message("Writing ssjdc_spatial_criteria.csv")
  write_csv(x = fl, file = file.path(pth$gpc1, "ssjdc_spatial_criteria.csv")) 
} else {
  message("Loading hand-selected ssjdc_spatial_criteria.csv, which specifies which spatial files to include.")
  fl <- read_csv(file.path(pth$gpc1, "ssjdc_spatial_criteria.csv")) %>%
    filter(load == T) 
}

### E. ssjdc. Now load each file. ----
sf_list <- pmap(fl, function(...){
  args <- enexprs(...)
  message(str_glue("Now reading {args$dir}"))
  
  # get project id
  ProjectID <- args$ProjectID
  
  # get layer names
  lyrs <- st_layers(args$path)$name
  
  # deal with multiple spatial object layers.
  # prefer polygon > lines > points
  if(length(lyrs) > 1){
    # prefer lines over points
    types_ <- st_layers(args$path)$geomtype %>%
      unlist()
    polygons <- str_detect(types_, regex("polygon", ignore_case = T))
    lines <- str_detect(types_, regex("line", ignore_case = T))
    if(T %in% polygons){
      lyrs <- lyrs[polygons]
    } else if(T %in% lines){
      lyrs <- lyrs[lines]
    }
  } 
  
  # read layer
  obj <- st_read(dsn = args$path,
                 layer = lyrs) 
  
  if(nrow(obj) == 0){
    return(NULL)
  }
  
  # crs transform
  obj <- obj %>%
    st_transform(crs = crs_$crs_epsg)
  
  # Add ID
  obj <- obj %>%
    mutate(nceas_id = paste0("ssjdc", "_", ProjectID, "_", row_number()))
  
  # only one attr of interest: PopupInfo
  if("PopupInfo" %in% names(obj)){
    obj$PopupInfo <- map_vec(
      obj$PopupInfo,
      function(x) {
        if(is.na(x)){
          return(NA_character_)
        }
        if (str_detect(x, "\\<.+\\>")) {
          x %>%
            rvest::read_html() %>%
            rvest::html_text() %>%
            str_trim()
        } else
        {
          x
        }
      })
    
    obj <- obj %>%
      select(nceas_id, PopupInfo)
    
  } else {
    obj <- obj %>%
      select(nceas_id)
  }
  
  obj <- obj %>%
    mutate(ProjectID = ProjectID) %>%
    select(ProjectID, nceas_id, everything())
  
  # return
  obj
})

# Separate spatial from attrs
# begin with attrs
tmp <- map_dfr(sf_list, function(x){
  x %>%
    st_set_geometry(NULL)
})

tmp <- tmp %>%
  mutate(PopupInfo = ifelse(PopupInfo == "", 
                            NA_character_, 
                            PopupInfo)) 
# finish attrs
ssjdc_atts <- tmp %>%
  left_join(ssjdc_tb, 
            by = "ProjectID")

# extract sf
ssjdc_sf <- map(sf_list, function(x){
  x <- x %>%
    select(nceas_id) 
  
  # Get the geometry type of the layer
  geom_type <- st_geometry_type(x, by_geometry = FALSE)
  
  # Check if the geometry type includes Z or M dimensions
  if(grepl("Z|M", geom_type)) {
    # Remove Z and/or M dimensions
    x <- st_zm(x, drop = TRUE)
  }
  
  x <- st_make_valid(x)
  
  x %>%
    mutate(geometry_type = st_geometry_type(x))
})

### ssjdc write ----
write_csv(x = ssjdc_atts,
          file = file.path(pth$gpc1, "ssjdc_atts.csv"))


### spatial 

ssjdc_geomtypes <- map(ssjdc_sf, 
                       ~.x$geometry_type %>% 
                         as.character() %>% 
                         unique()
) %>%
  unlist() %>%
  unique()

# delete previous version
map(ssjdc_geomtypes, function(type_){
  st_delete(dsn = pth$gpc1_gpkg,
            layer = paste0("ssjdc_", type_))
})

walk(ssjdc_sf, function(obj){
  
  objs <- obj %>%
    group_split(geometry_type)
  
  walk(objs, function(x){
    geomtype <- x$geometry_type %>% unique()
    layer_ <- paste0("ssjdc_", geomtype)
    st_write(
      obj = x,
      dsn = pth$gpc1_gpkg,
      layer = layer_,
      append = T
    )
  })
  
})

logger("Finished creating singular spatial database.")