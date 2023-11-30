# R version 4.3.1 (2023-06-16)
# CNRA (Bond Accountability) Scraping Script 1.
# Author: KT
# Purpose: Scrapes Program Pages. This script is intended only to scrape 
# relevant html pages to cloud storage. It is not intended to extract data from 
# these sites.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# initialize libs, paths, etc.
source("data_processing/cnra_scrape/init_cnra_scrape.R")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load links to program pages.
xl <- read_xlsx(
  path = file.path(path_branch, "Funding Programs and Contacts.xlsx")
) %>%
  filter(!is.na(`Bond Accountability link`))

links <- xl$`Bond Accountability link` 

# Create session for current page.
session <- bow(url = "https://bondaccountability.resources.ca.gov", 
               user_agent = "Restoration Group")

# Scrape and save the html. This html file doesn't include all images that you would get if you went to your browser and saved the site.
walk(links,
    function(x){
      p <- url_parse(x)
      id <- p$query$ProgramAllocationPK
      print(id)
      # using {polite} set up session
      html <- nod(bow = session, path = "/P1Program.aspx") %>%
        scrape(bow = .,
               query = p$query)
      
      write_html(x = html,
                 file = file.path(path_prog, paste0(id, ".html")))
    })

meta_query <- map_dfr(links,
    function(x){
      p <- url_parse(x)
      tibble(
        ProgramAllocationPK = p$query$ProgramAllocationPK,
        Program = p$query$Program,
        ProgramPK = p$query$ProgramPK,
        PropositionPK = p$query$PropositionPK
      )
    })

write_csv(meta_query,
          file = file.path(path_prog, "metadata.csv"))
