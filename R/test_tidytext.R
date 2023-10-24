### test text extraction from pdfs with initial docs from Delta Conservancy
### compare word differences in each type of doc
### Pascale 10/23/23

# library
library(pdftools)
library(readtext)
library(tidytext)
library(dplyr)
library(ggplot2)
library(tm)
library(ggpubr)

# upload pdf
dir <- "data_raw" # names location of pdf files
files <- list.files(dir, pattern = "*.pdf$", full.names = TRUE) # creates vector of pdf file names

# get data (in Plain Text UTF-8)
tidy_pdf <- readtext(files)

# make text data
tidy_words <- tidy_pdf %>%
  unnest_tokens(word, text)

# remove stop words
tidy_stop_words <- tidy_words %>%
  anti_join(stop_words) # without stop words (reduced by 17,591)

# remove numbers and symbols
tidy_stop_num_words <- mutate(tidy_stop_words, word = gsub(x = word, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

tidy_stop_num_words <-  tidy_stop_num_words[!(is.na(tidy_stop_num_words$word) | tidy_stop_num_words$word ==""), ] # reduced by another 3,233

# check
check <- tidy_stop_num_words %>%
  count(word, sort = TRUE)

check  %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkred") +
  xlab(NULL) +
  ylab("Word Count") +
  coord_flip() +
  ggtitle("Word Usage")

# look at each pdf separately and compare top words
staff_rec <- tidy_stop_num_words %>%
  filter(doc_id == "AI 8.6 Prop 1-1605_Staff Recommendation_final.pdf") %>%
  count(word, sort = TRUE) # 613

proposal <- tidy_stop_num_words %>%
  filter(doc_id == "P1-1605_Full Proposal.pdf") %>%
  count(word, sort = TRUE) # 2194

report <- tidy_stop_num_words %>%
  filter(doc_id == "P1-1605_ProgressReport_P1-1605_Deliv 1g_Final Report.pdf") %>%
  count(word, sort = TRUE) # 835

contract <- tidy_stop_num_words %>%
  filter(doc_id == "Prop 1-1605 A2_Fully Executed_4-15-20.pdf") %>%
  count(word, sort = TRUE) # 2202

a <- staff_rec  %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkred") +
  xlab(NULL) +
  ylab("Word Count") +
  coord_flip() +
  ggtitle("Staff Recommendation")

b <- proposal  %>%
  filter(n > 40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkblue") +
  xlab(NULL) +
  ylab("Word Count") +
  coord_flip() +
  ggtitle("Full Proposal")

c <- report  %>%
  filter(n > 8) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkgreen") +
  xlab(NULL) +
  ylab("Word Count") +
  coord_flip() +
  ggtitle("Final Report")

d <- contract  %>%
  filter(n > 40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkorange") +
  xlab(NULL) +
  ylab("Word Count") +
  coord_flip() +
  ggtitle("Fully Executed")


ggarrange(a, b, c, d,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

# look for benefit
staff_rec %>%
  filter(word == "benefit" | word == "benefits" | word == "beneficial")

# proposal is highest (53), then staff rec (12), contract (5) and report (1)

# cdfw example

monitor <- tidy_stop_num_words %>%
  filter(doc_id == "P1596027FinalAnnualMonitoringReport.pdf") %>%
  count(word, sort = TRUE) # 2749

cdfw_proposal <- tidy_stop_num_words %>%
  filter(doc_id == "P1596027-Proposal.pdf") %>%
  count(word, sort = TRUE) # 2633

final_report <- tidy_stop_num_words %>%
  filter(doc_id == "P1596027-FinalGrantReport-2020-0420.pdf") %>%
  count(word, sort = TRUE) # 1123

agreement <- tidy_stop_num_words %>%
  filter(doc_id == "P1596027_GrantAgreement-2016-0630.pdf") %>%
  count(word, sort = TRUE) # 1662

e <- monitor  %>%
  filter(n > 40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "pink") +
  xlab(NULL) +
  ylab("Word Count") +
  coord_flip() +
  ggtitle("Monitoring Report")

f <- cdfw_proposal  %>%
  filter(n > 40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "lightblue") +
  xlab(NULL) +
  ylab("Word Count") +
  coord_flip() +
  ggtitle("Proposal")

g <- final_report  %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "lightgreen") +
  xlab(NULL) +
  ylab("Word Count") +
  coord_flip() +
  ggtitle("Final Grant Report")

h <- agreement  %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "#FFC073") +
  xlab(NULL) +
  ylab("Word Count") +
  coord_flip() +
  ggtitle("Grant Agreement")

ggarrange(e, f, g, h,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

agreement %>%
  filter(word == "benefit" | word == "benefits" | word == "beneficial")

# monitor (1), cdfw_proposal (27), final_report (6), agreement (6, 1 typo)
