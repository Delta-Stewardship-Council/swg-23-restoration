
#' Converts sf object with geometry to tibble with lon/lat columns
#' 
#' @param sf_obj is an object of class "sf" and "data.frame".
#' @return object of class data.frame
to_coords <- function(sf_obj){
  # input validation
  stopifnot(inherits(sf_obj, "sf"))
  
  # convert sf object to standard coords CRS 4326
  sf_obj <- sf_obj %>%
    st_transform(crs = 4326)
  
  # first get type before proceeding.
  type_ <- sf::st_geometry_type(sf_obj) %>%
    unique() %>%
    as.character()
  
  if(identical(type_, "POINT")){
    coords <- sf_obj %>%
      st_geometry() %>%
      st_coordinates()
    
  } else if(identical(type_, "MULTIPOLYGON")){
    centroids <- sf_obj %>%
      st_make_valid() %>%
      st_centroid()
    
    coords <- centroids %>%
      st_geometry() %>%
      st_coordinates()
  } else {
    # not a POINT obj
    coords <- sf_obj %>%
      st_centroid() %>%
      st_geometry() %>%
      st_coordinates()
  }
  
  # remove geometry
  obj <- sf_obj %>%
    st_set_geometry(NULL)
  
  if("geometry_type" %in% names(obj)){
    # remove descriptive column
    obj <- obj %>%
      select(-geometry_type)
  }
  
  obj$lon <- coords[,1]
  obj$lat <- coords[,2]
  
  # return
  obj
}

#' Convert tibble with lon/lat columns into an SF object with point geometry.
#'
#' @param tb is a tibble or data.frame with lon/lat columns.
#' @param crs_ is either an integer specifying an EPSG code, or a wkt (ie. input for `sf::st_transform`)
#' @return object of class "sf" and "data.frame"
coords_to_sf <- function(tb, crs_ = 26910){
  # input validation
  stopifnot(inherits(tb, "data.frame"))
  
  # convert
  if("lon" %in% names(tb)){
    sf_obj <- tb %>%
      st_as_sf(coords = c("lon", "lat"), 
               crs = 4326)
  } else if ("long" %in% names(tb)){
    sf_obj <- tb %>%
      st_as_sf(coords = c("long", "lat"), 
               crs = 4326)
  } else if("longitude" %in% names(tb)){
    sf_obj <- tb %>%
      st_as_sf(coords = c("longitude", "latitude"), 
               crs = 4326)
  } else {
    stop("No lon/lat columns present.")
  }
  
  if(!is.null(crs_)){
    sf_obj <- sf_obj %>%
      st_transform(crs = crs_)
  }
  
  # return
  sf_obj
}

