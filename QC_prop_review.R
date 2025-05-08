setwd("C:/Users/pgoertle/OneDrive - California Department of Water Resources")

file <- read.csv("all_restoration_data_cleaned_proj_names.csv")
head(file)
colnames(file)<-"name"

library(dplyr)
test <- file %>% group_by(name) %>% tally()

write.csv(test, "prop_name_num.csv")


# look at doc ids
doc <- read.csv("step1_lookup_updated.csv")
head(doc)

prop_name <- unique(file$name) #97

doc_id <- unique(doc$doc_id) #94

length(intersect(prop_name,doc_id)) #83

new <- intersect(prop_name,doc_id)
match <- rep("TRUE", 83)
new_dat <- cbind(new, match)
colnames(new_dat)[1] <-"name"

lookup <- setdiff(doc_id, new)
match <- rep("lookup_only", 11)
lookup_dat <- cbind(lookup, match)
colnames(lookup_dat)[1] <-"name"

cleaned <- setdiff(prop_name, new)
match <- rep("cleaned_only", 14)
cleaned_dat <- cbind(cleaned, match)
colnames(cleaned_dat)[1] <-"name"

fin <- rbind(new_dat, lookup_dat, cleaned_dat)
write.csv(fin, "compare_names.csv")
