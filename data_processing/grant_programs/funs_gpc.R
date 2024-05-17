#' Used in `gpc1...R` script to transform tibbles into spatial data, then write
#' it and documentation to file.
#' 
#' @return NULL
write_to_gpkg <- function(
    # sf object for points data
  data_,
  # name of file
  nm_,
  # output dir path
  dir_,
  # output gpkg
  gpkg_,
  # output docs,
  docs_,
  # target crs
  target_crs_
){
  # Transform tibble to spatial data, and new columns
  tmp <- data_ %>%
    st_zm(drop = TRUE, what = "ZM") %>%
    st_transform(crs = target_crs_) %>%
    mutate(file_id = nm_,
           obj_id = row_number()) %>%
    # just to make filtering easier later,
    # we'll create a hash.
    mutate(md5 = digest(object = paste0(file_id, obj_id),
                        algo = "md5")) %>%
    select(file_id, obj_id, md5, everything())
  
  # Write spatial data. ----
  tmp %>%
    select(file_id, obj_id, md5) %>%
    st_write(
      obj = .,
      dsn = gpkg_,
      layer = nm_
    )
  
  # Write Attributes data ----
  # create file name
  attr_nm <- paste0(
    "attributes_",
    nm_,
    ".csv"
  )
  
  # write to storage
  tmp %>%
    st_set_geometry(NULL) %>%
    write_csv(
      file.path(
        dir_,
        attr_nm
      )
    )
  
  # Write Docs ----
  docs <- tibble(
    filename = attr_nm,
    description = str_glue("Contains attribute data for {nm_}. The file_id identifies the original raw data. The obj_id is the unique identifier by which you can join this data to the spatial data in the geopackage."),
    date_created = format(Sys.Date()),
    source_code = "creating_spatial_db.R"
  ) 
  
  write_csv(
    x = docs,
    file = docs_,
    append = T
  )
  
  invisible()
}

#' Create a new column of an sf object that denotes the size of the bounding box that would enfold the objects.
#' @return the sf object
add_bbox_area_to_sf <- function(sf_object) {
  # Ensure the object is an sf object
  if (!inherits(sf_object, "sf")) {
    stop("The provided object is not an sf object.")
  }
  
  bbox_area <- sf_object %>%
    # Calculate the bounding box
    st_bbox() %>%
    # Create a POLYGON from the bbox
    st_as_sfc() %>%
    # Calculate the area of the bounding box
    st_area()
  
  # Add the bbox area as a new column to the sf object
  sf_object$bbox_area <- as.numeric(bbox_area)
  
  return(sf_object)
}

#' Recursively run compound filter through data for specific values.
#' 
#' @param dat is the data.frame being filtered
#' @param crit is a data.frame containing two columns: variable and category, which is used to filter.
#' @return dat is the filtered data.frame
compound_filter <- function(dat, crit){
  # input validation
  stopifnot(inherits(dat, "data.frame"))
  stopifnot(inherits(crit, "data.frame"))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # get filter criteria
  # variable of interest
  var_ <- crit$variable[1]
  # categories of interest
  cats_ <- crit$category[1] %>%
    str_split_1(pattern = "\\,\\s")
  
  # error check
  stopifnot(var_ %in% names(dat))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # run the filter
  dat <- dat %>%
    # First, split the variable of interest from comma separated values to
    # a list-type column, containing a character vector for each observation.
    dplyr::mutate(tmpvar = stringr::str_split(
      !!rlang::sym(var_), 
      "\\,\\s")) %>%
    # Then, see if the categories of interest are present in the variable of
    # interest.
    dplyr::mutate(tmpvar = purrr::map(tmpvar, function(chr_){
      T %in% (cats_ %in% chr_)
    })) %>%
    # filter out rows/observations without our cats of interest
    dplyr::filter(tmpvar == T) %>%
    # drop filtering column
    dplyr::select(-tmpvar)
  
  # recursively apply function
  if(nrow(crit) > 1){
    dat <- compound_filter(dat = dat, 
                           crit = crit[2:nrow(crit),])
  }
  
  # return
  dat
}
