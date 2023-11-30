# R version 4.3.1 (2023-06-16)
# CNRA (Bond Accountability) Scraping Script 5.
# Author: KT
# Purpose: This script downloads spatial data.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# initialize libs, paths, etc.
source("web_scrape/cnra_scrape/init_cnra_scrape.R")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get metadata
meta_proj <- read_csv(file.path(path_proj, "metadata.csv")) %>%
  # remove duplicate projects.
  distinct(project_id)

# This is the base query, to which we'll attach parameters.
# This was base url is derived from examining Network traffic with a
# web browser.
url_base <- "https://gis.cnra.ca.gov"
url_path <- "/arcgis/rest/services/Economy/ABCRS/MapServer/{layer}/query"


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This is a function of {polite} to get the robots.txt.
session <- bow(url = url_base,
               user_agent = "Restoration Group",
               force = T)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Begin scraping.

# Do we need to re-run this code to catch missed geometries?
recheck <- F

# Get a list of files in the output directory
curr_sp <- list.files(path_spatial)

# Run scraping algo.
walk(seq_len(nrow(meta_proj)), function(i){
  # get project ID
  id <- meta_proj$project_id[i]
  
  if(recheck){
    regex <- paste0("^",id,"(?=\\_)")
    if(T %in% str_detect(curr_sp, regex)){
      # skip
      return()
    }
  }
  
  print(id)
  
  walk(2:0, function(layer){
    print(layer)
    geom_type <- switch(
      layer + 1,
      "POINT",
      "LINESTRING",
      "POLYGON"
    )
    
    # Create filename
    dsn_ <- str_glue("{id}_{geom_type}.geojson") %>%
      file.path(path_spatial, .)
    
    if(exists(dsn_)){
      # Skip if file exists.
      return()
    }
    
    # This just slips the geometry type into the query.
    # In other words, it replaces {layer} with 2, 1, or 0.
    current_path <- url_path %>%
      str_glue()
    
    # This uses the settings devised by robots.txt
    received <- nod(bow = session, 
                    path = current_path) %>%
      # This scrapes using the 'current_url', 
      # but with the query parameters below.
      scrape(
        bow = .,
        query = list(
          # output file type.
          f = "geojson",
          # This identifies the project:
          where = str_glue("ProjectNo_FK = {id}"),
          returnGeometry = "true",
          spatialRel = "esriSpatialRelIntersects",
          # Get all attributes.
          outFields = "*"
        ),
        verbose = T
      )
    
    # NOTE
    # `received` is a raw, hexadecimal input stream. In other words,
    # it is encoded. You have two options with encoded data, you can either
    # use writeBin(received, "data.geojson"), or convert within the 
    # R environment, which is what we do below.
    
    # Assuming 'received' contains the raw response from the server
    # Convert raw data to character string
    geojson_text <- rawToChar(received)
    
    # If the server sends a BOM (Byte Order Mark), 
    # you might need to remove it
    geojson_text <- gsub("\xEF\xBB\xBF", "", geojson_text)
    
    # Convert list/data frame to an sf object
    geojson_sf <- st_read(geojson_text, 
                          quiet = TRUE, 
                          stringsAsFactors = FALSE)
    
    # Add info to meta_proj
    meta_proj[meta_proj$project_id == id,
              geom_type] <<- nrow(geojson_sf)
    
    # Write to file
    if(nrow(geojson_sf) > 0){
      st_write(obj = geojson_sf, 
               dsn = dsn_)
    }
    
  })
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write_csv(meta_proj,
          file = file.path(path_spatial, "spatial_directory.csv"))
