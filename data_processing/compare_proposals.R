## ------------------------------------------------ ##
#  Delta Restoration -- Comparing Similar Proposals
## ------------------------------------------------ ##
# Script author(s): Angel Chen

# Purpose:
## Compares and picks out proposals that are similar in content

## ------------------------------------------------ ##
#                  User Settings -----
## ------------------------------------------------ ##

# Select the agency of interest
agency <- "ssjdc"

# Select page range of interest
page_range <- 15:19

# Specify any "weird" proposals
weird_proposals <- c("P1-1801_Full Proposal.pdf")

## ------------------------------------------------ ##
#                  Housekeeping -----
## ------------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, LexisNexisTools, pdftools, qpdf, jsonlite, quanteda)

# Create necessary sub-folder(s)
dir.create(path = file.path("compare_proposals_result"), showWarnings = F)

# Point to data folder on Box (see Kenji's tutorial)
path_to_data <- jsonlite::read_json("paths.json")$box_path

## ------------------------------------------------ ##
#           Extracting Proposal Texts -----
## ------------------------------------------------ ##

# Identify all proposals
( all_raw_files <- dir(path = file.path(path_to_data, "grant_proposals", "raw", agency)) )

# Remove any "weird" proposals
# P1-1801_Full Proposal.pdf is removed for having an unusually short amount of pages compared to other proposals in ssjdc
raw_files <- all_raw_files[! all_raw_files %in% weird_proposals]

# Make an empty list to store extracted text from proposals
text_list <- list()

# For every proposal...
for (i in 1:length(raw_files)){
  # Grab its name
  raw_file_name <- raw_files[i]
  
  # Message processing start
  message("Extracting '", raw_file_name, "' (file ",  i, " of ", length(raw_files), ")")
  
  # Create a temporary file to store our subsetted proposal pdf
  tmp <- tempfile()
  
  # Subset the proposal and store it in tmp
  qpdf::pdf_subset(file.path(path_to_data, "grant_proposals", "raw", agency, raw_file_name), tmp, pages = page_range)
  
  # Extract text from the subsetted proposal
  txt_v1 <- pdftools::pdf_text(tmp)
  
  # Combine the vector of text together
  # And remove the new lines, new tabs, and bullet points
  txt_v2 <- purrr::reduce(txt_v1, paste) %>%
    gsub(pattern = "\\n", replacement = " ") %>%
    gsub(pattern = "\\t", replacement = " ") %>%
    gsub(pattern = "•", replacement = " ")
  
  # Add to list
  text_list[[raw_file_name]] <- txt_v2
  
  # Delete the subsetted proposal
  unlink(tmp)
}

# Turn our extracted proposal texts into a corpus
myCorpus <- quanteda::corpus(unlist(text_list))

## ------------------------------------------------ ##
#           Comparing Proposal Texts -----
## ------------------------------------------------ ##

# Compare the texts
results <- LexisNexisTools::lnt_similarity(myCorpus, rep(Sys.Date(), length(myCorpus)), threshold = 0.8)

# Tidy the results by selecting only the columns and rows of interest
sorted_results <- results %>%
  as.data.frame() %>%
  dplyr::select(ID_original, ID_duplicate, Similarity, rel_dist) %>%
  dplyr::arrange(rel_dist) %>%
  dplyr::slice(1:10) %>%
  dplyr::mutate(page_num = paste0(page_range[1], "-", page_range[length(page_range)]))

# Get the names of the original texts for easy comparison
names_of_docs <- c()
for (i in 1:length(sorted_results$ID_original)){
  # Grab the name
  name_of_doc <- names(myCorpus[sorted_results$ID_original[i]])
  
  # Add to our vector of names
  names_of_docs <- append(names_of_docs, name_of_doc)
}

# Get the names of the duplicate texts for easy comparison
more_names_of_docs <- c()
for (i in 1:length(sorted_results$ID_duplicate)){
  # Grab the name
  name_of_doc <- names(myCorpus[sorted_results$ID_duplicate[i]])
  
  # Add to our vector of names
  more_names_of_docs <- append(more_names_of_docs, name_of_doc)
}

# Add these names as columns to our table of results
# Rename and reorder as needed
sorted_named_results <- sorted_results %>%
  dplyr::bind_cols(as.data.frame(names_of_docs), as.data.frame(more_names_of_docs)) %>%
  dplyr::rename(name_original = names_of_docs) %>%
  dplyr::rename(name_duplicate = more_names_of_docs) %>%
  dplyr::relocate(name_original, .after = ID_duplicate) %>%
  dplyr::relocate(name_duplicate, .after = name_original) 
  
sorted_named_results

# Export results locally
write.csv(x = sorted_named_results, file = file.path("compare_proposals_result", paste0(agency,"_proposal_duplicates_pages_", unique(sorted_named_results$page_num), ".csv")), na = "", row.names = F)

# End ----