##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 7/10/2024, by Pascale
# Description: This investigates relationships between words: n-grams and correlations
#########################################################

# library
library(tidytext)
library(dplyr)
library(tidyr)
library(textdata)
library(ggplot2)
library(igraph)
library(ggraph)

# windows_paths.R must be run first

# tokenizing by n-gram
bigrams <- tidy_pdf %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 4) %>%
  filter(!is.na(bigram))

# counting and filtering n-grams
bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2", "word3", "word4"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)%>%
  filter(!word4 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, word3, word4, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, word3, word4, sep = " ")

# analyzing bigrams
# word before 'benefit' in each proposal
benefit <- bigrams_filtered %>%
  filter(word2 == "benefit") %>%
  count(doc_id, word1, word3, word4, sort = TRUE)

write.csv(benefit, "benefit_as_word2.csv")

benefit <- bigrams_filtered %>%
  filter(word1 == "benefit") %>%
  count(doc_id, word2, word3, word4, sort = TRUE)

write.csv(benefit, "benefit_as_word1.csv")

# term frequency (tf), how frequently a word occurs in a document
# inverse document frequency (idf), which decreases the weight for commonly used words and increases the weight for words that are not used very much in a collection of documents.
# tf-idf (the two quantities multiplied together), the frequency of a term adjusted for how rarely it is used
bigram_tf_idf <- bigrams_united %>%
  count(doc_id, bigram) %>%
  bind_tf_idf(bigram, doc_id, n) %>%
  arrange(desc(tf_idf))

# how often words are preceded by a word like “benefit”
benefit_wrod1 <- bigrams_separated %>%
  filter(word1 == "benefit") %>%
  count(word1, word2, word3, word4, sort = TRUE)

# sentiment analysis on the bigram data
# AFINN lexicon gives a numeric sentiment value for each word, with positive or negative numbers indicating the direction of the sentiment.
AFINN <- get_sentiments("afinn")

AFINN

benefit_words <- bigrams_separated %>%
  filter(word1 == "benefit") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

benefit_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"benefit\"")

social_words <- c("jobs", "safety", "education", "recreation")

social_benefit_words <- bigrams_separated %>%
  filter(word1 %in% social_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

social_benefit_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by") +
  facet_grid(.~word1)

# visualizing a network of bigrams with ggraph
# filter for only relatively common combinations
bigram_counts <- benefit_wrod1 %>%
  count(word1, word2, word3, word4, sort = TRUE)

bigram_graph <- bigram_counts %>%
  #filter(n >5) %>%
  graph_from_data_frame()

bigram_graph <- benefit[,-1] %>%
  #filter(n >5) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
