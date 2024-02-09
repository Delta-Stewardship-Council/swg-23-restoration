
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

if(!dir.exists(pth$out)){
  message("Creating deliverables output dir.")
  dir.create(pth$out)
}

# Create a logging function bespoke to this process.
logger <- make_log(
  .filepath = file.path(pth$out, "log.txt"),
  .purpose = "spatial db synthesis"
)

target_crs <- 26910

# Convert readme into a tibble for later processing.
readme <- readLines(con = file.path(pth$raw, "README.md"))

# Convert to tibble and remove white spaces.
readme <- tibble(
  txt = readme
) %>%
  filter(txt != "")

# reorganize README tibble.
readme <- readme %>%
  mutate(filename = 
           str_extract(txt, "(?<=File\\:\\s).+") %>%
           str_remove_all(., '\"')
           ) %>%
  fill(filename, .direction = "down") %>%
  filter(!is.na(filename)) %>%
  filter(str_detect(txt, "^File\\:", negate = T)) %>%
  mutate(item = str_extract(txt, "^[^:]+(?=:\\s)")) %>%
  mutate(txt = str_extract(txt, "(?<=:\\s).+")) %>%
  select(filename, item, txt)

# Loading Files ----

# Because each file in the directory has different ways to organize the spatial
# information, our first task is to read each item appropriately.

data <- list()

## A. CNRA_Project_Coordinates.xlsx ----
data[["CNRA_Project_Coordinates.xlsx"]] <- readxl::read_xlsx(
  file.path(pth$raw, "CNRA_Project_Coordinates.xlsx")) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

## B. ecoatlas_sfbrafunded_projectsandsites.csv ----
data[["ecoatlas_sfbrafunded_projectsandsites.csv"]] <- readr::read_csv(
  file = file.path(pth$raw, "ecoatlas_sfbrafunded_projectsandsites.csv"),
  show_col_types = F
) %>%
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = "WGS84",
           # There are projects without coords. Keep these!
           na.fail = F)

## C. ecoatlas_sfbrafunded_shapefile ----

data[["ecoatlas_sfbrafunded_shapefile"]] <- st_read(
  dsn = file.path(pth$raw, 
                  "ecoatlas_sfbrafunded_shapefile", 
                  "habitatprojects.shp")
)

## D. Prop 1 Projects_SCC.xls ----

data[["Prop 1 Projects_SCC.xls"]] <- readxl::read_xls(
  path = file.path(pth$raw, "Prop 1 Projects_SCC.xls") 
) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

## E. CDFW_Prop_1_68_GHG_Rest_Grants_....xlsx ----

data[["CDFW_Prop_1_68_GHG_Rest_Grants_Delta_data_20231113-no science.xlsx"]] <- readxl::read_xlsx(
  path = file.path(
    pth$raw, 
    "CDFW_Prop_1_68_GHG_Rest_Grants_Delta_data_20231113-no science.xlsx"
  )
) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


## F. DeltaConservancy_ProjectShapefiles ----

# This directory has multiple different shape files.
# Begin by reading all the data in.
dirs_ <- list.files(
  file.path(pth$raw, 
            "DeltaConservancy_ProjectShapefiles", 
            "For NCEAS"),
  full.names = T)

dir_nms <- str_remove(dirs_, ".+\\/")

dc_shp <- map(dirs_, function(.dir){
  flist <- list.files(.dir, full.names = T)
  shps <- flist[str_detect(flist, "\\.shp$")]
  # get the filename only
  shps_nms <- str_remove(shps,  ".+\\/")
  
  out <- map(shps, function(.shp){
    x <- sf::st_read(dsn = .shp, quiet = T)
    # Oddly, there are one or two shps with no spatial
    # or attribute information. We'll remove these by returning NULL.
    if(nrow(x) == 0){
      return(NULL)
    } else {
      return(x)
    }
  }) %>%
    set_names(shps_nms)
  
  # Remove null
  out <- out[map_vec(out, ~!is.null(.x))]
  
  # return
  out
}) %>%
  set_names(dir_nms)

# Storing Data ----

## Setup ----

# create path to geopackage
pth$gpkg <- file.path(pth$out, "grant_programs_spatial.gpkg")

# Remove old geopackage
if(file.exists(pth$gpkg)){
  message("Remove existing gpkg.")
  file.remove(pth$gpkg)
}

pth$docs <- file.path(pth$out, "documentation.csv")

# Remove old documentation
if(file.exists(pth$docs)){
  message("Remove existing docs.")
  file.remove(pth$docs)
}

docs <- tibble(
  filename = "grant_programs_spatial.gpkg",
  description = "Contains spatial data for 'grant_program_coordinates' and a key-value for joining purposes. All spatial data are stored in CRS EPSG: 6414. No filtering has been done on this spatial database.",
  date_created = format(Sys.Date()),
  source_code = "creating_spatial_db.R"
)

# Create documentation
write_csv(
  x = docs,
  file = pth$docs
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# No we go through each document (with a capitalized letter, eg. F),
# and write their attributes and spatial data.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## F. DeltaConservancy_ProjectShapefiles ----
# Starting with the most complex one.

# Next, we'll simplify this list into a few sf obj and write to gpkg.
walk2(dc_shp, names(dc_shp), function(.obj, .nm){
  # Begin creating core gpkg layer name.
  core_nm <- "DeltaConservancy_ProjectShapefiles"
  Sys.sleep(1)
  walk2(.obj, names(.obj), function(.obj2, .nm2){
    
    # get geom type
    geom_type <- .obj2 %>% 
      st_geometry_type() %>% 
      as.character() %>%
      unique() %>%
      paste0(collapse = "-")
    
    # make layer name
    lyr <- paste0(core_nm, ".", geom_type)
    file_id <- paste0(
      core_nm, "..", .nm, "..", .nm2
    )
    
    # Get the geom and give it an id.
    out <- .obj2 %>%
      st_geometry() %>%
      st_sf() %>%
      mutate(file_id = file_id, 
      obj_id = row_number()) %>%
      st_zm(drop = TRUE, what = "ZM") %>%
      st_transform(crs = 26910)
    
    if(!file.exists(pth$gpkg)){
      new <- T
    } else {
      new <- F
      lyrs <- tryCatch(st_layers(pth$gpkg)$name,
                       error = function(e){
                         message(str_glue("Error: {file_id}"))
                         stop(e)
                       })
    }
    
    # Now write to file.
    if(new){

      # Gpkg needs to be initiated
      st_write(
        obj = out,
        dsn = pth$gpkg,
        layer = lyr
      )
    } else if(lyr %in% lyrs){

      # Gpkg already exists,
      # And layer already exists
      st_write(
        obj = out,
        dsn = pth$gpkg,
        layer = lyr,
        append = T
      )
    } else {
      # Layer is new.

      st_write(
        obj = out,
        dsn = pth$gpkg,
        layer = lyr
      )
    }
    gc()
  })
})

# Now we'll extract the attributes from the shapefiles.
f_attrs <- map2_dfr(dc_shp, names(dc_shp), function(.obj, .nm){
  # Begin creating core gpkg layer name.
  core_nm <- "DeltaConservancy_ProjectShapefiles"
  
  map2_dfr(.obj, names(.obj), function(.obj2, .nm2){
    
    # get geom type
    geom_type <- .obj2 %>% 
      st_geometry_type() %>% 
      as.character() %>%
      unique() %>%
      paste0(collapse = "-")
    
    # make layer name
    lyr <- paste0(core_nm, ".", geom_type)
    file_id <- paste0(
      core_nm, "..", .nm, "..", .nm2
    )
    
    # Get the geom and give it an id.
    out <- .obj2 %>%
      st_set_geometry(NULL) %>%
      mutate(file_id = file_id, 
             obj_id = row_number()) %>%
      select(file_id, obj_id, everything())

    # return
    out
  })
})

write_csv(f_attrs,
          file.path(
            pth$out,
            "attributes_DeltaConservancy_ProjectShapefiles.csv"
          )
)



docs <- tibble(
  filename = "attributes_DeltaConservancy_ProjectShapefiles.csv",
  description = "Contains attribute data for 'DeltaConservancy_ProjectShapefiles'. The file_id contains nested information about the file structure of the original raw data. Each sub-folder is separated by two periods. The obj_id is the unique identifier by which you can join this data to the spatial data in the geopackage.",
  date_created = format(Sys.Date()),
  source_code = "creating_spatial_db.R"
)
  
write_csv(
  x = docs,
  file = pth$docs,
  append = T
)

## E. CDFW_Prop_1_68_GHG_Rest_Grants_....xlsx ----

# Here we make a general function to deal with simple point tables.

fn_simple <- function(
    # sf object for points data
    .data,
    # name of file
    .nm
){
  # Transform spatial data, and new columns
  tmp <- .data %>%
    st_zm(drop = TRUE, what = "ZM") %>%
    st_transform(crs = 26910) %>%
    mutate(file_id = .nm,
           obj_id = row_number()) %>%
    select(file_id, obj_id, everything())
  
  # Write spatial data.
  tmp %>%
    select(file_id, obj_id) %>%
    st_write(
      obj = .,
      dsn = pth$gpkg,
      layer = .nm
    )
  
  # Write Attributes data
  # create file name
  attr_nm <- paste0(
    "attributes_",
    .nm,
    ".csv"
  )
  
  # write to storage
  tmp %>%
    st_set_geometry(NULL) %>%
    write_csv(
      file.path(
        pth$out,
        attr_nm
      )
    )
  
  docs <- tibble(
    filename = attr_nm,
    description = str_glue("Contains attribute data for {.nm}. The file_id identifies the original raw data. The obj_id is the unique identifier by which you can join this data to the spatial data in the geopackage."),
    date_created = format(Sys.Date()),
    source_code = "creating_spatial_db.R"
  ) 
  
  write_csv(
    x = docs,
    file = pth$docs,
    append = T
  )
  
  invisible()
}

fn_simple(
  .data = data$`CDFW_Prop_1_68_GHG_Rest_Grants_Delta_data_20231113-no science.xlsx`,
  .nm = "CDFW_Prop_1_68_GHG_Rest_Grants_Delta_data_20231113-no science.xlsx"
)

## D. Prop 1 Projects_SCC.xls ----

fn_simple(
  .data = data$`Prop 1 Projects_SCC.xls`,
  .nm = "Prop 1 Projects_SCC.xls"
)

## C. ecoatlas_sfbrafunded_shapefile ----

fn_simple(
  .data = data$ecoatlas_sfbrafunded_shapefile,
  .nm = "ecoatlas_sfbrafunded_shapefile"
)

## B. ecoatlas_sfbrafunded_projectsandsites.csv ----

fn_simple(
  .data = data$`ecoatlas_sfbrafunded_projectsandsites.csv`,
  .nm = "ecoatlas_sfbrafunded_projectsandsites.csv"
)

## A. CNRA_Project_Coordinates.xlsx ----

fn_simple(
  .data = data$`CNRA_Project_Coordinates.xlsx`,
  .nm = "CNRA_Project_Coordinates.xlsx"
)

logger("Finished creating singular spatial database.")