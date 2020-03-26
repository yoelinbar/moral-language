### WARNING ###
# This script will not run as-is. It requires the full tweet text which we cannot post publicly for copyright reasons.
# It is included here to document what we did, and for our own use.

### LIBRARIES ###
library(here)
library(stm)
library(data.table)
library(dplyr)
library(psych)
library(tidyverse)
library(ggcorrplot)

# Utility functions
here <- here::here
source( here('Analysis Code', 'utils.R') )

### PRE-PROCESSING TWEETS ###
# Read in tweets
tweets <- fread( here('Congress Tweets', 'AllCongressTweets.csv'), data.table = FALSE )

# Merge DW-NOMINATE w/ tweet file
dwnominate115 <- fread( here('Congress Tweets', 'DWNOMINATE-115.csv'), data.table = FALSE  )
dwnominate114 <- fread( here('Congress Tweets', 'DWNOMINATE-114.csv'), data.table = FALSE  )

# Code which Congress(es) they were in
dwnominate115$C115 <- TRUE
dwnominate115$C114 <- ifelse( dwnominate115$twitter_handle %in% dwnominate114$twitter_handle, TRUE, FALSE )

# Need to do a little pre-processing on the 114th Congress. We drop those that are also in the 115th.
dwnominate114 <- dwnominate114[!dwnominate114$twitter_handle %in% dwnominate115$twitter_handle, ]

# Code which Congress(es) they were in
dwnominate114$C114 <- TRUE
dwnominate114$C115 <- FALSE

# We also need to recode party_code to affiliation variables then drop that field
# The affiliation variable already exists in dwnominate115
dwnominate114$affiliation <- ifelse(dwnominate114$party_code == 100, "D", "R")
dwnominate114<-within(dwnominate114, rm("party_code"))

# Merge Congresses
dwnominate<-rbind(dwnominate114, dwnominate115)

# Normalize twitter handle case
dwnominate$twitter_handle <- tolower(dwnominate$twitter_handle)

# Recode affiliation & dummy
dwnominate$Party <- ifelse(dwnominate$affiliation == "D", "Democratic", "Republican")
dwnominate$partyd <- as.numeric(dwnominate$Party == "Democratic")

# Join on twitter handle
tweets <- merge(tweets,dwnominate, by.x="author", by.y = "twitter_handle", all = FALSE)

# Write new column w/ dates in POSIX format
tweets$posixdate <- as.POSIXct(tweets$date, tz="GMT")

# 2016 Election Day: 11-08
tweets$election <- ifelse( tweets$posixdate<as.POSIXct("2016-11-09 00:00", tz="GMT"), 'pre', 'post' ) 

# limit to 2016 onwards
tweets <- tweets[tweets$posixdate >= as.POSIXct("2016-01-01 00:00"),]

# Keep only tweets & metadata columns
tweets_stm <- select(tweets, 'author', 'tweet_cleaned', 'affiliation', 'election', 'generated_id')

### TOPIC MODELLING ###
# TEXT PRE-PROCESSING
processed <- textProcessor(tweets_stm$tweet_cleaned, metadata = tweets_stm)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# FIT STM
# Using Lee & Mimno algorithm to select number of topics
tweet_model <- stm(documents = out$documents, vocab = out$vocab, K = 0, prevalence =~ affiliation + election, max.em.its = 500, data = out$meta, init.type = "Spectral", model = tweet_model)

# Inspect topics
labelTopics(tweet_model)

# Tweets that are representative for each topic
stm::findThoughts(tweet_model, texts=meta$tweet_cleaned)

# Metadata/topic relationships
prep <- estimateEffect(1:78 ~ affiliation + election, tweet_model, meta = out$meta, uncertainty = "Global")
summary(prep)

# Plot of topic frequency
plot(tweet_model, type = "summary", text.cex = 0.7)

# Plot by affiliation
plot(prep, covariate = "affiliation", model = tweet_model, method = "difference", cov.value1 = "D", cov.value2 = "R", verbose.labels = F, xlab = "Conservative                                         Liberal", main = "Effect of Liberal vs. Conservative")

# Pull out probabilities from STM model
tweet_probs <- tweet_model$theta
tweets_with_topic_probs <- merge(tweet_probs, meta, by.x = 0, by.y = 0)
tweets_with_topic_probs <- tweets_with_topic_probs[-1]

# Semantic coherence plot
topicQuality(tweet_model, documents = out$documents, xlab = "Semantic Coherence", ylab = "Exclusivity", label.cex = 0.5)

semcoh <- semanticCoherence(tweet_model,documents=out$documents, M=10)
exclusivity <- exclusivity(tweet_model, M=10)

semantic_coherence_plot = data.frame(semcoh, exclusivity)
ggplot(data = semantic_coherence_plot, aes(semcoh, exclusivity)) +
  geom_point() + xlab("Semantic Coherence") + ylab("Exclusivity")



# Read in DDR loadings file
loadings <- fread( here('DDR', 'document_dictionary_loadings.tsv'), sep = '\t', header = TRUE, data.table = FALSE )
colnames(loadings) <- c("generated_id", "Loyaltyvirtue", "Authorityvice", "Loyaltyvice", 
                        "Fairnessvice", "Carevirtue", "Authorityvirtue", "Purityvice",
                        "Purityvirtue", "Carevice", "Fairnessvirtue")

# Merge
data <- merge(tweets_with_topic_probs, loadings, by = "generated_id")

### CORRELATIONS BETWEEN TOPICS AND MORAL LOADINGS ###

corr <- psych::corr.test(data[ ,85:94],
                         data[ ,3:80], use = "pairwise", method = "pearson", adjust = "bonferroni")

ggcorrplot(as.data.frame(corr['r']))

# CORRELATIONS BY PARTY

# Filter Dem

data_D <- data %>% filter(affiliation == 'D')

psych::corr.test(data_D[ ,85:94],
                 data_D[ ,3:80], use = "pairwise", method = "pearson", adjust = "bonferroni")

# Filter R

data_R <- data %>% filter(affiliation == 'R')

psych::corr.test(data_R[ ,85:94],
                 data_R[ ,3:80], use = "pairwise", method = "pearson", adjust = "bonferroni")

