# R version 4.3.1 (2023-06-16)
# CNRA (Bond Accountability) Scraping Script 4.
# Author: KT
# Purpose: This script loads the project html files, extracts data, and stores 
# it as a json file. Note, spatial data is handled separately in scrape5.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# initialize libs, paths, etc.
source("data_processing/cnra_scrape/init_cnra_scrape.R")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load some functions to help parse text.
source("data_processing/cnra_scrape/funs_cnra_processing.R")

meta_proj <- read_csv(file.path(path_proj, "metadata.csv")) %>%
  # remove duplicate projects.
  distinct(project_id)

# This will take a minute.
walk(.x = seq_len(nrow(meta_proj)), function(i){
  # Obtain html
  pid <- meta_proj$project_id[i]
  proj <- read_html(file.path(path_proj, paste0(pid, ".html")))
  print(paste0(i, "-", pid))
  
  # Obtain containers
  roots_proj <- proj %>%
    # Note, by selecting this "class" of tags, we may end up with a 
    # list of multiple tags using this class. This is, after all,
    # the nature of css classes. As such, each element has to be treated
    # as a separate object using something like lapply or map.
    html_elements(".container_style_a")
  
  # Get Project Info
  strong <- fn_strong_text(roots_proj, "#ContentPlaceHolder1_ProgramView")
  
  # Get Funding Info.
  funding <- roots_proj %>%
    html_element("#ctl00_ContentPlaceHolder1_FundingGrid_ctl00")
  
  funding <- funding[!is.na(funding)] %>%
    html_table() %>%
    `[[`(1) %>%
    filter(`Contribution Sources` != "" & `Contribution Date` != "")
  
  # Metric
  metric <- roots_proj %>%
    html_element("#ctl00_ContentPlaceHolder1_MetricsGrid_ctl00")
  
  metric <- metric[!is.na(metric)]  %>%
    html_table() %>%
    `[[`(1)
  
  # Water Action Plan Target
  wapt <- roots_proj %>%
    html_element("#ctl00_ContentPlaceHolder1_WAPGrid_ctl00")
  
  wapt_cells <- wapt[!is.na(wapt)] %>%
    html_elements("td")
  
  wapt <- map_dfr(seq_along(wapt_cells), function(j){

    child <- wapt_cells[j] %>%
      html_children()
    
    if(length(child) > 0){
      out <- child %>%
        fn_1tag_extract()
    } else {
      out <- wapt_cells[j] %>%
        fn_1tag_extract()
    }
    out %>%
      mutate(item = j) %>%
      select(item, everything())
  })
  
  pack <- list(
    project_info = strong,
    funding = funding,
    metric = metric,
    wapt = wapt
  )
  
  write_json(pack, file.path(path_projjson, paste0(pid, ".json")))
})

