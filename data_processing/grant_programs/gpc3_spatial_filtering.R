# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up ----
# Get path to home.
if(!exists("path_home")){
  # home directory: swg-23-restoration
  print("Variable path_home created.")
  path_home <- getwd() %>%
    stringr::str_extract(., "(.+)((swg-23-restoration(?=\\/))|(swg-23-restoration$))") %>%
    stringr::str_split_1(., "\\/|\\\\{1,2}")
  path_home <- file.path(
    paste(
      path_home,
      collapse = .Platform$file.sep
    )
  )
}

source(file.path(path_home,
                 "data_processing", 
                 "grant_programs", 
                 "init_gpc.R"))

# Create a logging function bespoke to this process.
logger <- make_log(
  .filepath = file.path(pth$gpc2, "log.txt"),
  .purpose = "spatial filtering"
)

baydelta <- st_read(
  dsn = file.path(
    pth$data,
    "bay_delta_boundary",
    "StudyBoundaryFINAL.shp"
))

baydelta_buffer <- st_buffer(x = baydelta, dist = 5000)

baydelta_buffer$name <- "5km_buffer"

baydelta_buffer <- baydelta_buffer %>%
  select(name)

# get all project layers
lyrs_ <- st_layers(pth$gpc2_gpkg)$name

ids_ <- c()

# apply spatial filter on buffered frame
walk(lyrs_, function(lyr){
  obj <- st_read(dsn = pth$gpc2_gpkg,
          layer = lyr)
  
  sub_ <- obj %>%
    st_filter(baydelta_buffer)
  
  # save ids
  write(
    sub_$nceas_id %>% unique(),
    file = file.path(pth$gpc3, "all_nceas_ids.txt"),
    append = T
  )
  
  st_write(
    obj = sub_,
    dsn = pth$gpc3_gpkg,
    layer = lyr,
    delete_layer = T
  )
})

# load all ids
summary <- readLines(con = file.path(pth$gpc3, "all_nceas_ids.txt"))

# filter attribute tables based on spatial filter

tbls <- c("cnra", "sfbra", "cscc", "cdfw", "ssjdc")
tbls2 <- map(tbls, function(x){
  tb <- read_csv(file.path(pth$gpc2, paste0(x, "_atts.csv")))
  tb %>%
    filter(nceas_id %in% summary) %>%
    select(nceas_id, everything())
  })

walk2(tbls2, tbls, function(x, nm){
  write_csv(
    x = x,
    file = file.path(pth$gpc3, paste0(nm, "_atts.csv"))
  )
})
