# R version 4.3.1 (2023-06-16)
# CNRA (Bond Accountability) Data Cleaning.
# Author: KT
# Purpose: This script is to be run after all raw data downloaded. This script collects and organizes all data.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# initialize libs, paths, etc.
source("data_processing/cnra_scrape/init_cnra_scrape.R")

# load functions
source("data_processing/cnra_scrape/funs_cnra_processing.R")

# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Load libraries ----
# lapply(
#   X = c(
#     "readxl"
#   ),
#   FUN = function(x) {
#     if(!(x %in% .packages())){
#       print(paste0("Loading {", x, "}"))
#       library(x, character.only = T)
#     }
#   }
# ) |>
#   invisible()
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Programs ----
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Preparations.
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # Load links to program pages.
# xl <- read_xlsx(
#   path = file.path(path_branch, "Funding Programs and Contacts.xlsx")
# ) %>%
#   filter(!is.na(`Bond Accountability link`))
# 
# links <- tibble(
#   program_url = xl$`Bond Accountability link`
#   ) %>%
#   mutate(program_id = str_extract(program_url,
#                                   "(?<=ProgramAllocationPK\\=)\\d+")) %>%
#   select(program_id, program_url)
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Clean Program Information ----
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # get program file listing
# progs <- list.files(path_progjson, pattern = "\\.json$")
# 
# # extract IDs
# prognms <- str_extract(progs, "^\\d+(?=\\.)")
# 
# # get program info
# progs <- map2_dfr(progs, prognms, function(x, nm){
# 
#   x <- read_json(file.path(path_progjson, x))
# 
#   # remove project listing
#   x <- map_dfr(x$program_info, function(row){
#     as_tibble_row(row)
#   })
# 
#   fn_strong_format(x) %>%
#     mutate(program_id = nm) %>%
#     select(program_id, everything())
# })
# 
# # Widen and add program URLs
# progs <- progs %>%
#   pivot_wider(id_cols = "program_id",
#               names_from = "key",
#               values_from = "value") %>%
#   left_join(links, by = "program_id")
# 
# # Write
# write_csv(progs,
#           file = file.path(path_clean, "program_information.csv"))
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Projects ----
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Clean Project Information ----
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # get project file listing
# projs <- list.files(path_projjson, pattern = "\\.json$")
# 
# # extract IDs
# projnms <- str_extract(projs, "^\\d+(?=\\.)")
# 
# ### General Project Information ----
# # get project info
# proj_info <- map2_dfr(projs, projnms, function(x, nm){
#   print(nm)
#   x <- read_json(file.path(path_projjson, x))
# 
#   # remove project listing
#   strong <- map_dfr(x$project_info, function(row){
#     as_tibble_row(row)
#   })
# 
#   strong <- fn_strong_format(strong) %>%
#     mutate(project_id = nm) %>%
#     select(project_id, everything())
# 
#   strong
# })
# 
# proj_info <- proj_info %>%
#   pivot_wider(id_cols = "project_id",
#               names_from = "key",
#               values_from = "value")
# 
# # Write
# write_csv(
#   proj_info,
#   file.path(path_clean, "project_general_information.csv")
# )
# 
# ### Water Action Plan Target ----
# 
# wapt <- map2(projs, projnms, function(x, nm){
#   print(nm)
#   x <- read_json(file.path(path_projjson, x))
#   fn_wapt(x$wapt) %>%
#     mutate(project_id = nm) %>%
#     select(project_id, everything())
# })
# 
# wapt <- wapt %>%
#   bind_rows()
# 
# # Write
# write_csv(
#   wapt,
#   file.path(path_clean, "project_wapt.csv")
# )
# 
# ### Funding ----
# 
# funding <- map2(projs, projnms, function(x, nm){
#   print(nm)
#   x <- read_json(file.path(path_projjson, x))
# 
#   map_dfr(x$funding, function(row){
#     if(length(row) == 0){
#       return(
#         tibble(
#           `Contribution Sources` = NA,
#           `Contribution Date` = NA,
#           `Contribution Amount` = NA
#         )
#       )
#     }
#     as_tibble_row(row)
#   }) %>%
#     mutate(project_id = nm) %>%
#     select(project_id, everything())
# })
# 
# funding <- funding %>%
#   bind_rows()
# 
# # Write
# write_csv(
#   funding,
#   file.path(path_clean, "project_funding.csv")
# )
# 
# ### Metric ----
# 
# metric <- map2(projs, projnms, function(x, nm){
#   print(nm)
#   x <- read_json(file.path(path_projjson, x))
# 
#   map_dfr(x$metric, function(row){
#     if(length(row) == 0){
#       return(
#         tibble(
#           `Project Metric` = NA,
#           `Metric Topic` = NA,
#           Quantity = NA,
#           Unit = NA
#         )
#       )
#     }
#     as_tibble_row(row)
#   }) %>%
#     mutate(project_id = nm) %>%
#     select(project_id, everything())
# })
# 
# metric <- metric %>%
#   bind_rows()
# 
# # Write
# write_csv(
#   metric,
#   file.path(path_clean, "project_metric.csv")
# )
# 
# ## Spatial ----
# 
# files <- list.files(path_spatial, pattern = "\\.geojson$")
# 
# f_pt <- files[str_detect(files, "POINT")]
# f_ln <- files[str_detect(files, "LINESTRING")]
# f_pn <- files[str_detect(files, "POLYGON")]
# 
# list_sf <- list()
# 
# list_sf$point <- map_dfr(f_pt, function(file){
#   st_read(file.path(path_spatial, file))
# })
# 
# ln <- map(f_ln, function(file){
#   obj <- st_read(file.path(path_spatial, file))
#   nm <- colnames(obj)
#   nm <- nm[str_detect(string = nm,
#                 pattern = regex("geometry",
#                                 ignore_case = T),
#                 negate = T)]
#   obj %>%
#     mutate(across(.cols = all_of(nm),
#                   .fns = as.character))
# })
# 
# types <- map_vec(ln, function(x){
#   st_geometry_type(x) %>%
#     unique() %>%
#     as.character()
# })
# 
# list_sf$multilinestring <- ln[types == "MULTILINESTRING"] %>%
#   bind_rows()
# list_sf$linestring <- ln[types == "LINESTRING"] %>%
#   bind_rows()
# 
# pn <- map(f_pn, function(file){
#   obj <- st_read(file.path(path_spatial, file))
#   nm <- colnames(obj)
#   nm <- nm[str_detect(string = nm,
#                       pattern = regex("geometry",
#                                       ignore_case = T),
#                       negate = T)]
#   obj %>%
#     mutate(across(.cols = all_of(nm),
#                   .fns = as.character))
# })
# 
# types <- map_vec(pn, function(x){
#   st_geometry_type(x) %>%
#     unique() %>%
#     as.character()
# })
# 
# list_sf$multipolygon <- pn[types == "MULTIPOLYGON"] %>%
#   bind_rows()
# list_sf$polygon <- pn[types == "POLYGON"] %>%
#   bind_rows()
# 
# 
# file.remove(file.path(path_clean, "projects_spatial.gpkg"))
# 
# walk2(list_sf, names(list_sf), function(x, nm){
#   st_write(
#     obj = x,
#     dsn = file.path(path_clean, "projects_spatial.gpkg"),
#     layer = nm
#   )
# })
# 
# # Copy spatial metadata to clean
# file.copy(
#   from = file.path(path_spatial, "spatial_directory.csv"),
#   to = file.path(path_clean, "spatial_directory.csv")
# )
# 
# # Key ----
# # This just copies one of the metadata files.
# key <- read_csv(file.path(path_proj, "metadata.csv"))
# 
# write_csv(key,
#           file.path(path_clean, "program_project_key.csv"))
