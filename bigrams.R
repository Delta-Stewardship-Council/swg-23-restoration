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

# remove numbers and symbols
bigrams_filtered1 <- mutate(bigrams_filtered, word1 = gsub(x = word1, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))
bigrams_filtered2 <- mutate(bigrams_filtered1, word2 = gsub(x = word2, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))
bigrams_filtered3 <- mutate(bigrams_filtered2, word3 = gsub(x = word3, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))
bigrams_filtered4 <- mutate(bigrams_filtered3, word4 = gsub(x = word4, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

bigrams_filtered_fin <- bigrams_filtered4[!(is.na(bigrams_filtered4$word1) | bigrams_filtered4$word1==""), ]
bigrams_filtered_fin <- bigrams_filtered_fin[!(is.na(bigrams_filtered_fin$word2) | bigrams_filtered_fin$word2==""), ]
bigrams_filtered_fin <- bigrams_filtered_fin[!(is.na(bigrams_filtered_fin$word3) | bigrams_filtered_fin$word3==""), ]
bigrams_filtered_fin <- bigrams_filtered_fin[!(is.na(bigrams_filtered_fin$word4) | bigrams_filtered_fin$word4==""), ]

# new bigram counts:
bigram_counts <- bigrams_filtered_fin %>%
  count(word1, word2, word3, word4, sort = TRUE)

bigrams_united <- bigrams_filtered_fin %>%
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

# Taylor's community analysis idea
# hypothesis - phrases with "community" will be social-themed (vs ecological) in proposals located within disadvantaged areas (CDC definition).
# how often words are preceded by a word like “community”
community_wrod1 <- bigrams_filtered_fin %>%
  filter(word1 == "community")

community_wrod1_sov <- merge(community_wrod1, sov_dat, by = "doc_id")

# community as first word, and SOV category 1
sum_wrod1_cat1 <- community_wrod1_sov %>%
  group_by(category1) %>%
  count(word1, word2, word3, word4, sort = TRUE)

# community as first word, and SOV category 2
sum_wrod1_cat2 <- community_wrod1_sov %>%
  group_by(category2) %>%
  count(word1, word2, word3, word4, sort = TRUE)

# community as first word, and SOV category 3
sum_wrod1_cat3 <- community_wrod1_sov %>%
  group_by(category3) %>%
  count(word1, word2, word3, word4, sort = TRUE)

# community as first word, and SOV category 4
sum_wrod1_cat4 <- community_wrod1_sov %>%
  group_by(category4) %>%
  count(word1, word2, word3, word4, sort = TRUE)

# category 1 = low
dat_test <- sum_wrod1_cat2 %>%
  filter(category2 == "low")

bigram_graph <- dat_test[,-1] %>%
  filter(n > 1) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# category 1 = high
dat_test <- sum_wrod1_cat2 %>%
  filter(category2 == "high")

bigram_graph <- dat_test[,-1] %>%
  filter(n > 1) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


# community as second word
community_wrod2 <- bigrams_filtered_fin %>%
  filter(word2 == "community")

community_wrod2_sov <- merge(community_wrod2, sov_dat, by = "doc_id")

# community as second word, and SOV category 1
sum_word2_cat1 <- community_wrod2_sov %>%
  group_by(category1) %>%
  count(word1, word2, word3, word4, sort = TRUE)

sum_word2_cat2 <- community_wrod2_sov %>%
  group_by(category2) %>%
  count(word1, word2, word3, word4, sort = TRUE)

sum_word2_cat3 <- community_wrod2_sov %>%
  group_by(category3) %>%
  count(word1, word2, word3, word4, sort = TRUE)

sum_word2_cat4 <- community_wrod2_sov %>%
  group_by(category4) %>%
  count(word1, word2, word3, word4, sort = TRUE)

# category 1 = low
dat_test <- sum_word2_cat2 %>%
  filter(category2 == "low")

bigram_graph <- dat_test[,-1] %>%
  filter(n > 1) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# category 1 = high
dat_test <- sum_word2_cat2 %>%
  filter(category2 == "high")

bigram_graph <- dat_test[,-1] %>%
  filter(n > 1) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


# community as third word
community_wrod3 <- bigrams_filtered_fin %>%
  filter(word3 == "community")

community_wrod3_sov <- merge(community_wrod3, sov_dat, by = "doc_id")

# community as third word, and SOV category 1
sum_word3_cat1 <- community_wrod3_sov %>%
  group_by(category1) %>%
  count(word1, word2, word3, word4, sort = TRUE)

sum_word3_cat2 <- community_wrod3_sov %>%
  group_by(category2) %>%
  count(word1, word2, word3, word4, sort = TRUE)

sum_word3_cat3 <- community_wrod3_sov %>%
  group_by(category3) %>%
  count(word1, word2, word3, word4, sort = TRUE)

sum_word3_cat4 <- community_wrod3_sov %>%
  group_by(category4) %>%
  count(word1, word2, word3, word4, sort = TRUE)

# category 3 = low
dat_test <- sum_word3_cat3 %>%
  filter(category3 == "low")

bigram_graph <- dat_test[,-1] %>%
  filter(n > 1) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# category 3 = high
dat_test <- sum_word3_cat3 %>%
  filter(category3 == "high")

bigram_graph <- dat_test[,-1] %>%
  filter(n > 1) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


# community as fourth word
community_wrod4 <- bigrams_filtered_fin %>%
  filter(word4 == "community")

community_wrod4_sov <- merge(community_wrod4, sov_dat, by = "doc_id")

# community as third word, and SOV category 1
sum_word4_cat1 <- community_wrod4_sov %>%
  group_by(category1) %>%
  count(word1, word2, word3, word4, sort = TRUE)

sum_word4_cat2 <- community_wrod4_sov %>%
  group_by(category2) %>%
  count(word1, word2, word3, word4, sort = TRUE)

sum_word4_cat3 <- community_wrod4_sov %>%
  group_by(category3) %>%
  count(word1, word2, word3, word4, sort = TRUE)

sum_word4_cat4 <- community_wrod4_sov %>%
  group_by(category4) %>%
  count(word1, word2, word3, word4, sort = TRUE)

# category 4 = low
dat_test <- sum_word4_cat4 %>%
  filter(category4 == "low")

bigram_graph <- dat_test[,-1] %>%
  filter(n > 1) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# category 4 = high
dat_test <- sum_word4_cat4 %>%
  filter(category4 == "high")

bigram_graph <- dat_test[,-1] %>%
  filter(n > 1) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


# try a different plot
bigrams_sov <- merge(bigrams_filtered_fin, sov_dat, by = "doc_id")

community2_word1 <- bigrams_sov %>%
  filter(category1 == "low") %>%
  filter(word2 %in% "community") %>%
  inner_join(AFINN, by = c(word1 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

community2_word1$order <- "before"
colnames(community2_word1)[1] <- "word"

community2_word3 <- bigrams_sov %>%
  filter(category1 == "low") %>%
  filter(word2 %in% "community") %>%
  inner_join(AFINN, by = c(word3 = "word")) %>%
  count(word3, word2, value, sort = TRUE)

community2_word3$order <- "after"
colnames(community2_word3)[1] <- "word"

low_community2 <- rbind(community2_word3,community2_word1)
##

community2_word1 <- bigrams_sov %>%
  filter(category1 == "high") %>%
  filter(word2 %in% "community") %>%
  inner_join(AFINN, by = c(word1 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

community2_word1$order <- "before"
colnames(community2_word1)[1] <- "word"

community2_word3 <- bigrams_sov %>%
  filter(category1 == "high") %>%
  filter(word2 %in% "community") %>%
  inner_join(AFINN, by = c(word3 = "word")) %>%
  count(word3, word2, value, sort = TRUE)

community2_word3$order <- "after"
colnames(community2_word3)[1] <- "word"

high_community2 <- rbind(community2_word3,community2_word1)
high_community2[11,4] <- 2
high_community2 <- high_community2[-12,]

high_community2 %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(n * value, word, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by") +
  facet_grid(.~word2) +
  theme(text = element_text(size=20))

# test if filter is working
community2_word1 <- bigrams_sov %>%
  filter(category1 == "low") %>%
  filter(word2 %in% "community") %>%
  inner_join(AFINN, by = c(word1 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

bigrams_sov_low <- subset(bigrams_sov, category1 == "low")

community2_word1_test <- bigrams_sov_low %>%
  filter(word2 %in% "community") %>%
  inner_join(AFINN, by = c(word1 = "word")) %>%
  count(word1, word2, value, sort = TRUE)



