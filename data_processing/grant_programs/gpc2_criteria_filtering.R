# The purpose of this script is to run the filtering criteria (created by
# Taylor).
# THIS SCRIPT FOLLOWS gpc1...R

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
  .purpose = "filtering"
)

crit <- read_xlsx(
  path = file.path(pth$gpc, 
                   "filter_criteria_update_022224.xlsx"),
  sheet = 1
)

# A. cnra ----

# It appears that there is no filter for CNRA, 
# so directly copy.

file.copy(
  from = file.path(pth$gpc1, "cnra_atts.csv"),
  to = file.path(pth$gpc2, "cnra_atts.csv")
)

cnra_sf <- st_read(pth$gpc1_gpkg,
                   layer = "cnra")

## write ----
st_write(
  obj = cnra_sf,
  dsn = pth$gpc2_gpkg,
  layer = "cnra",
  delete_layer = T
)

# B. sfbra ----

## load ----

sfbra_atts <- read_csv(file.path(pth$gpc1, "sfbra_atts.csv"))

## criteria ----

sfbra_crit <- crit %>%
  filter(str_detect(file, "sfbra"))

## att filter ----

sfbra_atts2 <- compound_filter(sfbra_atts, sfbra_crit)

## spatial filter ----

# retrieve matching IDs
sfbra_ids <- sfbra_atts2 %>%
  filter(!is.na(nceas_id)) %>%
  pull(nceas_id) %>%
  unique()

# Now filter out sfbra spatial
sfbra_sf <- st_read(
  dsn = pth$gpc1_gpkg,
  layer = "sfbra"
)

sfbra_sf2 <- sfbra_sf %>%
  filter(nceas_id %in% sfbra_ids)

## write ----
write_csv(
  x = sfbra_atts2,
  file = file.path(pth$gpc2, "sfbra_atts.csv")
)

st_write(
  obj = sfbra_sf2,
  dsn = pth$gpc2_gpkg,
  layer = "sfbra",
  delete_layer = T
)

# C. cscc ----

## load ----

cscc_atts <- read_csv(file.path(pth$gpc1, "cscc_atts.csv"))

## criteria ----

cscc_crit <- crit %>%
  filter(str_detect(file, "SCC"))

## att filter ----

cscc_atts2 <- compound_filter(cscc_atts, cscc_crit)

## spatial filter ----

# retrieve matching IDs
cscc_ids <- cscc_atts2 %>%
  filter(!is.na(nceas_id)) %>%
  pull(nceas_id) %>%
  unique()

# Now filter out cscc spatial
cscc_sf <- st_read(
  dsn = pth$gpc1_gpkg,
  layer = "cscc"
)

cscc_sf2 <- cscc_sf %>%
  filter(nceas_id %in% cscc_ids)

## write ----
write_csv(
  x = cscc_atts2,
  file = file.path(pth$gpc2, "cscc_atts.csv")
)

st_write(
  obj = cscc_sf2,
  dsn = pth$gpc2_gpkg,
  layer = "cscc",
  delete_layer = T
)

# D. cdfw ----

## load ----

cdfw_atts <- read_csv(file.path(pth$gpc1, "cdfw_atts.csv"))

## criteria ----

cdfw_crit <- crit %>%
  filter(str_detect(file, "CDFW"))

## att filter ----

cdfw_atts2 <- compound_filter(cdfw_atts, cdfw_crit)

## spatial filter ----

# retrieve matching IDs
cdfw_ids <- cdfw_atts2 %>%
  filter(!is.na(nceas_id)) %>%
  pull(nceas_id) %>%
  unique()

# Now filter out cdfw spatial
cdfw_sf <- st_read(
  dsn = pth$gpc1_gpkg,
  layer = "cdfw"
)

cdfw_sf2 <- cdfw_sf %>%
  filter(nceas_id %in% cdfw_ids)

## write ----
write_csv(
  x = cdfw_atts2,
  file = file.path(pth$gpc2, "cdfw_atts.csv")
)

st_write(
  obj = cdfw_sf2,
  dsn = pth$gpc2_gpkg,
  layer = "cdfw",
  delete_layer = T
)

# E. ssjdc ----

## load ----

ssjdc_atts <- read_csv(file.path(pth$gpc1, "ssjdc_atts.csv"))

## criteria ----

ssjdc_crit <- crit %>%
  filter(str_detect(file, "ECP"))

## att filter ----

ssjdc_atts2 <- compound_filter(ssjdc_atts, ssjdc_crit)

## spatial filter ----

# retrieve matching IDs
ssjdc_ids <- ssjdc_atts2 %>%
  filter(!is.na(nceas_id)) %>%
  pull(nceas_id) %>%
  unique()

ssjdc_layers <- st_layers(dsn = pth$gpc1_gpkg)$name %>%
  `[`(str_detect(., "ssjdc"))

walk(ssjdc_layers, function(lyr){
  message(lyr)
  
  sf <- st_read(dsn = pth$gpc1_gpkg,
          layer = lyr)
  
  sf <- sf %>%
    filter(nceas_id %in% ssjdc_ids)
  
  if(nrow(sf) > 0){
    st_write(
      obj = sf,
      dsn = pth$gpc2_gpkg,
      layer = lyr,
      delete_layer = T
    )
  }
})

write_csv(
  x = ssjdc_atts2,
  file = file.path(pth$gpc2, "ssjdc_atts.csv")
)