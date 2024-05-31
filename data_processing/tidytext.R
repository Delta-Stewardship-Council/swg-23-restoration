##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 5/31/2024, by Pascale
# Description: This takes pdfs, dissolves to text, removes unwanted words/symbols, and creates some exploratory plots
#########################################################

# library
library(tidytext)
library(dplyr)
library(ggplot2)

# windows_paths.R must be run first

# make text data
tidy_words <- tidy_pdf %>%
  unnest_tokens(word, text)

# remove stop words
tidy_stop_words <- tidy_words %>%
  anti_join(stop_words) # without stop words (reduced by 17,591)

# remove numbers and symbols
tidy_stop_num_words <- mutate(tidy_stop_words, word = gsub(x = word, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

tidy_stop_num_words <-  tidy_stop_num_words[!(is.na(tidy_stop_num_words$word) | tidy_stop_num_words$word ==""), ] # reduced by another 3,233

# plot most common words
check <- tidy_stop_num_words %>%
  count(word, sort = TRUE)

check  %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkred") +
  xlab(NULL) +
  ylab("Word Count") +
  coord_flip() +
  ggtitle("Word Usage")

# look for benefit
ben <- tidy_stop_num_words %>%
  filter(word == "benefit" | word == "benefits" | word == "beneficial")

length(unique(ben$doc_id)) # all

# look at a single example
prop_1713 <- tidy_stop_num_words %>%
  filter(doc_id == "P1-1713_Full Proposal.pdf") %>%
  count(word, sort = TRUE)

prop_1713  %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "#FFC073") +
  xlab(NULL) +
  ylab("Word Count") +
  coord_flip() +
  ggtitle("P1-1713")

