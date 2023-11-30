# R version 4.3.1 (2023-06-16)
# CNRA (Bond Accountability) Scraping Script 2.
# Author: KT
# Purpose: This reorganizes the information in the html files and stores it as 
# a json.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# initialize libs, paths, etc.
source("web_scrape/cnra_scrape/init_cnra_scrape.R")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load some functions to help parse text.
source("web_scrape/cnra_scrape/funs_cnra_processing.R")

meta_query <- read_csv(file.path(path_prog, "metadata.csv"))
  
# Process all programs.
walk(meta_query$ProgramAllocationPK, function(id){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load html
  file <- paste0(id, ".html")
  prog <- read_html(file.path(path_prog, file))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse html
  roots_prog <- prog %>%
    html_elements(".container_style_a")
  
  tb_strong <- fn_strong_text(roots_prog,
                              "#ContentPlaceHolder1_ProgramView")
  
  rows <- roots_prog %>%
    html_element("#ctl00_ContentPlaceHolder1_ProjectGrid") %>%
    html_element("tbody") %>%
    html_elements("tr")
  
  base_url <- "https://bondaccountability.resources.ca.gov"
  
  projects <- map_dfr(seq_along(rows), function(i){
    item <- rows[i] %>%
      html_elements("a")
    
    # return
    tb <- tibble(
      title = item %>%
        html_attr("title"),
      href = item %>%
        html_attr("href")
    )
    
    if(T %in% str_detect(tb$href, "^\\.")){
      tb$href <- str_replace(tb$href, "^\\.", base_url)
    }
    # Return
    tb
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Pack and Write to Storage
  pack <- list(
    program_info = tb_strong,
    projects = projects
  )
  
  write_json(pack, file.path(path_progjson, paste0(id, ".json")))
})











