# Title: bay-delta_merge_code
# Author(s): Bailey D. Morrison
# Contributor(s): Eric Holmes, Kenji M. Tomari
# Date: Nov 7, 2023



# Required Packages -------------------------------------------------------
require(sf)
require(nngeo)
require(foreach)
require(dplyr)
require(tools)


# Bay-Delta Shapefile Merge Function --------------------------------------
create_BayDelta_shp <- function(outdir){
  # URL locations for necessary shapefile downloads
  urls <- c("https://filelib.wildlife.ca.gov/Public/BDB/GIS/BIOS/Public_Datasets/500_599/ds586.zip",
           "https://www.sfei.org/sites/default/files/data/Delta_Historical_Ecology_GISdata_SFEI_ASC_2012.zip",
           "http://stacks.stanford.edu/file/druid:vw878wn8422/data.zip")
  file_names <- basename(urls) %>% # remove directory from file names
    tools::file_path_sans_ext() # remove post-fix
  
  temp <- tempdir() # create tmp dir to download shapefiles into
  setwd(temp) # set working directory to temp drive
  
  # Download, unzip shapefiles, and remove zipfiles
  print("Downloading shapefiles for merge")
  foreach::foreach(i = 1:length(urls)) %do% {
    download <- download.file(urls[i], 
                  destfile = paste0(temp, "\\", file_names[i], ".zip"),
                  mode = "wb",
                  quiet = TRUE)
    unzip_files <- unzip(paste0(temp, "\\", file_names[i], ".zip"), 
          exdir = paste0(temp, "\\", file_names[i]))
  }
  file.remove(paste0(temp, "\\", file_names, ".zip"), recursive = TRUE)
  
  # Read in shapefiles and correct for polygon class and merge issues
  print("Reading in shapefiles")
  bay <- sf::st_read("data/sfei_baylands_bndry.shp", 
                    quiet = TRUE)
  eco <- sf::st_read("Delta_Historical_Ecology_GISdata_SFEI_ASC_2012/Delta_Historical_Ecology_SFEI_ASC_2012.gdb", 
                    layer = "historical_habitats_Delta", 
                    promote_to_multi = TRUE,
                    quiet = TRUE) %>%
    sf::st_cast(to = "MULTIPOLYGON") %>% # remove MULTISURFACE Polygons
    sf::st_union(by_feature = FALSE) %>% # Merge internal polygons
    nngeo::st_remove_holes() %>% # remove random holes in center of polygon
    sf::st_transform(crs = st_crs(bay)) # reproject to same CRS as bay (NAD83 UTM Zone10N)
  legal <- sf::st_read(dsn = "ds586/ds586.gdb",
                      quiet = TRUE) %>%
    sf::st_transform(st_crs(bay)) 
  
  unlink(temp) # close tmp directory connection
  setwd("~.") # move working directory back to default
  
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


# Test code to run function -----------------------------------------------
outdir <- "C://Users/bdmor/Desktop/"
test <- create_BayDelta_shp(outdir)
plot(st_geometry(test))

