# R version 4.3.1 (2023-06-16)
# CNRA (Bond Accountability) Scraping Script 3.
# Author: KT
# Purpose: This script loads the program information (json files),
# and scrapes each project.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# initialize libs, paths, etc.
source("web_scrape/cnra_scrape/init_cnra_scrape.R")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

meta_query <- read_csv(file.path(path_prog, "metadata.csv"))

# Create session for current page.
session <- bow(url = "https://bondaccountability.resources.ca.gov", 
               user_agent = "Restoration Group")

walk(meta_query$ProgramAllocationPK, function(id){
  cat(paste0("~~~~~~~~~~~~~~~~~~~~~~~\n", 
             id, 
             "\n~~~~~~~~~~~~~~~~~~~~~~~\n"))
  
  # path to json file.
  file <- file.path(path_progjson, paste0(id, ".json"))
  
  # load json
  prog <- read_json(file)

  # convert to tibble.
  proj_tb <- map_dfr(prog$projects, function(row){
    as_tibble_row(row)
  })
  
  # begin scrape of each project
  walk(seq_len(nrow(proj_tb)), function(row_id){
    p <- url_parse(proj_tb$href[row_id])
    proj_id <- p$query$ProjectPK
    print(proj_id)
    
    # using {polite} set up session
    html <- nod(bow = session, path = p$path) %>%
      scrape(bow = .,
             query = p$query)
    
    write_html(x = html,
               file = file.path(path_proj, paste0(proj_id, ".html")))
  })
  
})

meta_proj <- map_dfr(seq_len(nrow(meta_query)), function(i){
  id <- meta_query$ProgramAllocationPK[i]
  # path to json file.
  file <- file.path(path_progjson, paste0(id, ".json"))
  
  # load json
  prog <- read_json(file)
  
  # convert to tibble.
  projectpk <- map_dfr(prog$projects, function(row){
    tibble(
      program_id = id,
      project_id = url_parse(row$href)$query$ProjectPK,
      title = row$title
    )
  })
})

write_csv(
  x = meta_proj,
  file = file.path(path_proj, "metadata.csv")
)
