##########################################################
# Created by: Kenji Tomari (ktomari@ucdavis.edu)
# Last updated: 5/31/2024, by Pascale
# Updated again by Eric on 6/24/2024
# Description: This script connects to Box, accesses pdfs and then reads in the files
# added extraction of specific proposal sections
#########################################################

# library
library(tidyverse)
library(stringr)
library(pdftools)
library(readtext)

## Set display options to remove scientific notation
options(scipen = 999)

# this will be different for each computer, see Kenji's 4/5/2024 email
# the last folder will be updated during 2024, June 6 meeting
path1 <- "C:/Users/eholmes/Box/NCEAS - Restoration/data/grant_proposals/raw/sfbra/test"

path1 <- str_split_1(path1, "\\/|\\\\{1,2}")

path1 <- file.path(
  paste(
    path1,
    collapse = .Platform$file.sep
  )
)

files <- list.files(path1, full.names = T, recursive = T, pattern = "*.pdf$")

tidy_pdf <- readtext(files)

tidy_pdf_projdesc <- data.frame()

for(i in 1:nrow(tidy_pdf)){
  
  thing <- str_split(tidy_pdf$text[i], pattern = "Project Description.")
  print(str(thing))
  proj_desc <- str_split(thing[[1]][length(thing[[1]])], pattern = "Site Description.")
  tidy_pdf_projdesc <- rbind(tidy_pdf_projdesc, data.frame(doc_id = tidy_pdf$doc_id[i],
                                                           text = proj_desc[[1]][1]))
  
}


step1new <- readxl::read_excel("C:/Users/eholmes/Box/NCEAS - Restoration/data/composite_data/step1_lookup.xlsx") %>% data.frame()

# step1 <- read.csv("C:/Users/eholmes/Box/NCEAS - Restoration/data/composite_data/step1.csv")
# 
# step1_lookup <- step1[duplicated(step1$project_title) == F,]
# write.csv(step1_lookup, file = "C:/Users/eholmes/Box/NCEAS - Restoration/data/composite_data/step1_lookup.csv", row.names = F)

## Janky nested for-loop to extract the proper text sections from proposals for analysis ----


## Proposal agencies: c("cdfw", "cnra", "cscc", "sfbra", "ssjdc")

path1 <- "C:/Users/eholmes/Box/NCEAS - Restoration/data/grant_proposals/final_selection"

path1 <- str_split_1(path1, "\\/|\\\\{1,2}")

path1 <- file.path(
  paste(
    path1,
    collapse = .Platform$file.sep
  )
)

files <- list.files(path1, full.names = T, recursive = T, pattern = "*.pdf$")

tidy_pdf <- readtext(files)

## SSJDC nested for loop for extracting porposal text sections

tidy_pdf_extract <- data.frame()

for(agency in c("cdfw", "cnra", "cscc", "sfbra", "ssjdc")){
  
  print(paste(agency, "-------------------------"))
  
  s1sub <- step1new[step1new$agency == agency,]
  
  if(agency == "ssjdc"){
    
    ## Extract abstract from SSJDC ----
    ## Problems: proposals with prefix 18 and 20 do not have abstracts
    
    for(proposal in step1new[step1new$agency == agency,"doc_id"]){
      
      print(paste(agency, "-", proposal, "abstract"))
      
      thing <- str_split(tidy_pdf[tidy_pdf$doc_id == proposal, "text"], pattern = "anticipated outcomes")
      print(length(thing[[1]]))
      nchar
      if(length(thing[[1]]) == 3){proj_desc <- str_split(thing[[1]][length(thing[[1]])], pattern = "Project Description: Purpose and Implementation")}
      else(proj_desc <- str_split(thing[[1]][length(thing[[1]])], pattern = "Applicant Information"))
      
      tidy_pdf_extract <- rbind(tidy_pdf_extract, 
                                data.frame(
                                  agency = agency,
                                  doc_id = proposal,
                                  section = "abstract",
                                  text = proj_desc[[1]][1]))
    }
    
    ## Extract project description from SSJDC ----
    
    for(proposal in step1new[step1new$agency == agency,"doc_id"]){
      
      print(paste(agency, "-", proposal, "project description"))
      
      thing <- str_split(tidy_pdf[tidy_pdf$doc_id == proposal, "text"], pattern = "Project Description: Purpose and Implementation")
      # thing <- str_split(tidy_pdf[tidy_pdf$doc_id == proposal, "text"], pattern = "Literature Cited.")
      print(length(thing[[1]]))
      
      proj_desc <- str_split(thing[[1]][length(thing[[1]])], pattern = "Environmental Compliance")
      
      tidy_pdf_extract <- rbind(tidy_pdf_extract, 
                                data.frame(
                                  agency = agency,
                                  doc_id = proposal,
                                  section = "project description",
                                  text = proj_desc[[1]][1]))
    }
    
    ## Extract outcomes and benefits from SSJDC ----
    
    for(proposal in step1new[step1new$agency == agency,"doc_id"]){
      
      print(paste(agency, "-", proposal, "outcomes and benefits"))
      
      thing <- str_split(tidy_pdf[tidy_pdf$doc_id == proposal, "text"], pattern = "survival stressors")
      print(length(thing[[1]]))
      
      proj_desc <- str_split(thing[[1]][length(thing[[1]])], pattern = "Local Support")
      
      tidy_pdf_extract <- rbind(tidy_pdf_extract, 
                                data.frame(
                                  agency = agency,
                                  doc_id = proposal,
                                  section = "outcomes and benefits",
                                  text = proj_desc[[1]][1]))
    }
    
    ## Extract Community Support & Integration from SSJDC ----
    ## Problems with comm support & integration: missing from later proposals
    
    for(proposal in step1new[step1new$agency == agency,"doc_id"]){
      
      print(paste(agency, "-", proposal, "Community Support & Integration"))
      
      thing <- str_split(tidy_pdf[tidy_pdf$doc_id == proposal, "text"], pattern = "identified partnerships")
      print(length(thing[[1]]))
      
      proj_desc <- str_split(thing[[1]][length(thing[[1]])], pattern = "Partnerships")
      
      tidy_pdf_extract <- rbind(tidy_pdf_extract, 
                                data.frame(
                                  agency = agency,
                                  doc_id = proposal,
                                  section = "community integration",
                                  text = proj_desc[[1]][1]))
    }
    
  }
  
}

tidy_pdf_extract$characters <- nchar(tidy_pdf_extract$text)

ggplot(tidy_pdf_extract, aes(x = characters, fill = section)) + 
  geom_histogram() + scale_fill_brewer(palette = "Set1")

