# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load path to cloud directory
source("admin_scripts/init_load_paths.R")

library(tidyverse)
library(readxl)
library(sf)

# create path to grant program coordinates directory
pth$gpc <- file.path(pth$data, "grant_program_coordinates")

# path to deliverables
pth$out <- file.path(pth$gpc, "deliverables")

# path to archive
pth$arc <- file.path(pth$out, "archived")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert criteria into a tibble for later processing.
crit <- read_xlsx(path = file.path(pth$gpc, "criteria.xlsx"))

# Unfortunately, the categories are all in one cell, comma separated.
# So, we'll split those up into multiple rows.
crit <- pmap_dfr(crit, function(...){
  args <- enexprs(...)
  
  cat <- str_split_1(args$category, ",\\s?")
  tibble(
    file = args$file,
    variable = args$variable,
    category = cat
  )
})

# Move all files in deliverables to 'archived' because they were intermediate files.
items <- list.files(pth$out)

if(!("archived" %in% items)){
  dir.create(file.path(pth$arc))
  
  walk(items, function(x){
    file.copy(
      from = file.path(pth$out, x),
      to = file.path(pth$arc, x)
    )
  })
  
  file.remove(file.path(pth$out, items))
}

items <- list.files(pth$arc)

# Criteria Filter ----

## Attribute Tables ----

fl <- crit$file %>% unique()

atts_ <- map(fl, function(file_){
  # read attributes
  atts <- read_csv(file = file.path(
    pth$arc, 
    paste0("attributes_", file_, ".csv")
    ))
  print(nrow(atts))
  
  # get specific criteria we're interested in.
  crit1 <- crit %>%
    filter(file == file_)
  
  # what variables are we interested in?
  var1 <- crit1$variable %>% unique()
  if(length(var) > 1){
    stop("issue with criteria filter var")
  }
  
  # what values for those variables are we interested in?
  cat1 <- crit1$category
  
  # filter
  atts %>%
    filter(!!sym(var1) %in% cat1)
})

# Now lets write these attribute tables to file.
walk(atts_, function(x){
  fname <- x$file_id[1] %>%
    str_remove(pattern = "\\.[[:alpha:]]{3,4}$") %>%
    paste0(., ".csv")
  write_csv(x = x, file = file.path(pth$out, 
                                    fname))
})

# Finally, based on communication with Taylor,
# we want all of DeltaConservancy_ProjectShapefiles
file.copy(
  from = file.path(pth$arc, "attributes_DeltaConservancy_ProjectShapefiles.csv"),
  to = file.path(pth$out, "DeltaConservancy_ProjectShapefiles.csv")
)

# Its sort of redundant, but now we're going to read the attribute tables
# back into R. Why? Because I want to match the file name to each list obj.
att_files <- list.files(path = pth$out, pattern = "\\.csv")
atts_ <- map(att_files, function(file_){
  read_csv(file = file.path(pth$out, file_))
})

atts_names <- att_files %>%
  str_remove("\\.csv$")

atts_ <- atts_ %>%
  set_names(nm = atts_names
  )

## Filter Spatial ----
lyrs <- st_layers(dsn = file.path(pth$arc, "grant_programs_spatial.gpkg"))

walk(lyrs$name, function(lyr){
  # is there an atts_ that matches this layer?
  fltr <- map_vec(atts_names, function(root){
    str_detect(lyr, root)
  })
  
  if(T %in% fltr){
    # get spatial data into R
    sf_ <- st_read(dsn = file.path(pth$arc, 'grant_programs_spatial.gpkg'),
            layer = lyr)
    
    # get sf atts
    sf_att <- sf_ %>%
      st_set_geometry(NULL)
    
    # now we have the problem of filtering. 
    # we could do a join, but thats complicated.
    # what we'll do instead is just paste0 each file_id and obj_id together.
    
    sf_id <- sf_att %>%
      mutate(filter_id = paste0(file_id, obj_id)) %>%
      pull(filter_id)
    
    # do the same for atts_
    atts_id <- atts_[[which(fltr)]] %>%
      mutate(filter_id = paste0(file_id, obj_id)) %>%
      pull(filter_id)
    
    matches_ <- which(sf_id %in% atts_id)
    
    if(length(matches_) == 0){
      return(NULL)
    }
    
    # now filter
    sf_ <- sf_[matches_,]
    
    st_write(
      obj = sf_,
      dsn = file.path(pth$out, "final_grant_programs_spatial.gpkg"),
      layer = lyr
    )
  }
})
