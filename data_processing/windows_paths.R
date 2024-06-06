##########################################################
# Created by: Kenji Tomari (ktomari@ucdavis.edu)
# Last updated: 5/31/2024, by Pascale
# Description: This script connects to Box, accesses pdfs and then reads in the files
#########################################################

# library
library(stringr)
library(pdftools)
library(readtext)

# this will be different for each computer, see Kenji's 4/5/2024 email
# the last folder will be updated during 2024, June 6 meeting
path1 <- "C:\\Users\\pgoertle\\Box\\NCEAS - Restoration\\data\\grant_proposals\\raw\\ssjdc"

path1 <- str_split_1(path1, "\\/|\\\\{1,2}")

path1 <- file.path(
  paste(
    path1,
    collapse = .Platform$file.sep
  )
)

files <- list.files(path1, full.names = T, recursive = T, pattern = "*.pdf$")

tidy_pdf <- readtext(files)


