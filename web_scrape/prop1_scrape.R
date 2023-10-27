# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(readxl)
library(digest)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Paths
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creates file.paths.
# home directory: swg-23-restoration
path_to_home <- getwd() %>%
  str_extract(., "(.+)((swg-23-restoration(?=\\/))|(swg-23-restoration$))")

path_to_data <- file.path(path_to_home, "data")

path_to_input <- file.path(path_to_data, "input_files")

# Load functions
source(file.path(path_to_home, "web_scrape", "funs_scraping.R"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create directories.
# Make sure all the paths exist
if(!dir.exists(path_to_input)){
  print("Creating path to data.")
  dir.create(path_to_input)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load xlsx of links
meta <- read_xlsx(
  file.path(
    path_to_input, 
    "Funding Programs and Contacts.xlsx")
  ) %>%
  # remove NA links
  filter(!is.na(`Bond Accountability link`))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Scrape the index of projects.
idx <- map(meta$`Bond Accountability link`, 
           function(url){
  print(url)
  session <- polite::bow(
    url = url,
    user_agent = "NCEAS Restoration Group"
  )
  scrape(session)
})

# as many of the links are relative links, store base url.
base_url <- "https://bondaccountability.resources.ca.gov"

# Generate a table of links
meta2 <- imap_dfr(idx, function(x, i){
  tibble(
    page1 = meta$`Bond Accountability link`[i],
    project_name = x %>%
      html_element("#ProgramContent") %>%
      html_elements("a") %>%
      html_attr("title"),
    project_link = x %>%
      html_element("#ProgramContent") %>%
      html_elements("a") %>%
      html_attr("href") %>%
      str_replace_all(., pattern = "^\\.", base_url)
  )
})

# retrieve metadata
meta2 <- imap_dfr(idx, function(x, i){
  tibble(
    page1 = meta$`Bond Accountability link`[i],
    metadata_item1 = x %>%
      html_element("#ContentPlaceHolder1_ProgramView") %>%
      html_elements("td") %>%
      html_elements("strong") %>%
      html_text2(),
    metadata_value1 = x %>%
      html_element("#ContentPlaceHolder1_ProgramView") %>%
      html_elements("td") %>%
      html_elements("span") %>%
      html_text2(),
    html_id = x %>%
      html_element("#ContentPlaceHolder1_ProgramView") %>%
      html_elements("td") %>%
      html_elements("span") %>%
      html_attr("id")
  )
})

idx[[2]] %>%
  html_element("#ContentPlaceHolder1_ProgramView") %>%
  html_elements("td") %>%
  html_elements("strong") %>%
  html_text2()

ch <- idx[[2]] %>%
  html_element("#ContentPlaceHolder1_ProgramView") %>%
  html_elements("td") %>%
  html_element("font")

map(ch, function(x){
  x %>%
    html_children() %>%
    html_name()
})


map(tmp, function(x){
  x %>%
    html_name()
})

node <- idx[[2]] %>%
  html_element("#ContentPlaceHolder1_ProgramView")

tmp1 <- fn_strong_text(idx[[2]], "#ContentPlaceHolder1_ProgramView")

node %>%
  html_name()

fn_wordseq(nm, "strong")



div1 <- "#ProjectContent"
div1 <- "#ProgramContent"

nm <- idx[[2]] %>%
  html_element(div1) %>% 
  html_element("table") %>%
  html_element("td") %>%
  html_element("font") %>%
  html_children() %>%
  html_name()

tmp <- idx[[2]] %>%
  html_element(div1) %>% 
  html_element("table") %>%
  html_element("td") %>%
  html_element("font")

out <- list()
strong <- NA
for(i in seq_along(nm)){
  if(nm[i] == "strong"){
    
  }
}




