# Title: bay-delta_merge_code
# Author(s): Bailey D. Morrison
# Contributor(s): Eric Holmes, Kenji Tomari
# Date: Nov 9, 2023

# Bay-Delta Shapefile Merge Function --------------------------------------
#' This function downloads spatial data necessary to create the Bay-Delta 
#' boundary used in this project. It then merges the spatial data.
#' 
#' @param outdir a character of length 1 with the path for the output directory.
#' @param urls a character of length 1 with the file path for the url document. 
#' The default path is relative to the swg-restoration project.
create_BayDelta_shp <- function(
    outdir,
    url_path = "data_processing/bay_delta_boundary/urls.json"
){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # URL locations for necessary shapefile downloads
  # (Note, `urls` is a named list: bay, eco, legal)
  urls <- read_json(url_path)
  
  file_names <- basename(unlist(urls)) %>% # remove directory from file names
    tools::file_path_sans_ext() # remove post-fix
  
  temp <- tempdir() # create tmp dir to download shapefiles into
  
  # create dirs
  foreach(i = 1:length(urls)) %do% {
    tmp_path <- file.path(temp, names(urls)[i])
    if(!dir.exists(tmp_path)){
      dir.create(tmp_path)
    }
  }
  
  # create file paths to temp directory for each download
  zip_paths <- file.path(temp, 
                         names(urls),
                         basename(unlist(urls)))
  
  unzipped_paths <- file.path(temp, 
                              names(urls),
                              file_names) %>%
    as.list() %>%
    set_names(names(urls))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Download, unzip shapefiles, and remove zipfiles
  print("Downloading shapefiles for merge")
  foreach::foreach(i = 1:length(urls)) %do% {
    
    # download spatial data
    if(!file.exists(zip_paths[i])){
      download.file(
        url = urls[[i]], 
        destfile = zip_paths[i],
        mode = "wb",
        quiet = TRUE)
    } else {
      print("Zipped file already exists.")
    }
    
    # unzip spatial data
    unzip_files <- unzip(
      zipfile = zip_paths[i], 
      exdir = unzipped_paths[[i]])
  }
  
  # clean up zipped files (optional?)
  file.remove(zip_paths, recursive = TRUE)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read in shapefiles and correct for polygon class and merge issues
  print("Reading in shapefiles")
  
  # Note, assumes third url is `bay` data
  bay <- sf::st_read(
    dsn = file.path(unzipped_paths$bay, "sfei_baylands_bndry.shp"), 
    quiet = TRUE)
  
  print("As of testing date (2023-11-9), {sf} must be 1.0-15 or else st_cast on MULTISURFACE yields an error. The version on CRAN is behind. Install the new version of {sf} from github, remotes::install_github('r-spatial/sf').")
  eco <- sf::st_read(
    dsn = file.path(
      unzipped_paths$eco, 
      "Delta_Historical_Ecology_SFEI_ASC_2012.gdb"), 
    layer = "historical_habitats_Delta", 
    promote_to_multi = TRUE,
    quiet = TRUE) %>%
    sf::st_cast(to = "MULTIPOLYGON") %>% # remove MULTISURFACE Polygons
    sf::st_union(by_feature = FALSE) %>% # Merge internal polygons
    nngeo::st_remove_holes() %>% # remove random holes in center of polygon
    sf::st_transform(crs = st_crs(bay)) # reproject to same CRS as bay (NAD83 UTM Zone10N)
  
  legal <- sf::st_read(
    dsn = file.path(
      unzipped_paths$legal, 
      "ds586.gdb"),
    quiet = TRUE) %>%
    sf::st_transform(st_crs(bay)) 
  
  unlink(temp) # close tmp directory connection
  
  # Combine shapefiles into one single polygon 
  print("Merging shapefiles into one polygon shapefile")
  bay_legal_union <- suppressWarnings(sf::st_union(bay, legal)) # union Bay and Legal Delta
  sfBay_eco_union <- suppressWarnings(sf::st_union(bay_legal_union, eco)) # union Bay, Legal Delta, and Eco Delta
  
  Bay_EcoLegalDelta <- sf::st_union(sfBay_eco_union, by_feature = FALSE) # Remove multi-polygons and convert to single polygon 
  
  # write final shapefile to output directory
  print("Writing shapefile to output directory")
  sf::st_write(Bay_EcoLegalDelta, 
               dsn = outdir, 
               layer = "Bay_EcoLegalDelta", 
               driver = "ESRI Shapefile", 
               append = FALSE,
               quiet = TRUE)
  
  return(Bay_EcoLegalDelta) # return final merged shapefile
}
