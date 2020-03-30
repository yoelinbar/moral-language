# This script merges tweet-level data, author-level data, and DDR output to produce the data file that is uploaded to GitHub
# Input files:
# Tweets: "AllCongressTweets.csv"
# Member of Congress data: "DWNOMINATE-115.csv", "DWNOMINATE-114.csv"
# DDR output: "document_dictionary_loadings.tsv"
#
# Output: tweets.csv

### REQUIREMENTS ###
library(here)
library(data.table)

# Make sure we are calling the right here()
here <- here::here

### PREPROCESSING ###
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

# Read in DDR output file
loadings <- fread( here('DDR', 'document_dictionary_loadings.tsv'), sep = '\t', header = TRUE, data.table = FALSE )
colnames(loadings) <- c("generated_id", "Loyaltyvirtue", "Authorityvice", "Loyaltyvice", 
                        "Fairnessvice", "Carevirtue", "Authorityvirtue", "Purityvice",
                        "Purityvirtue", "Carevice", "Fairnessvirtue")

# Merge
tweets <- merge(loadings,tweets,by="generated_id", all.x = FALSE, all.y = TRUE)

# Log- transform retweets/likes
tweets$retweets_T <- log(tweets$retweets + 1)
tweets$favorites_T <- log(tweets$favorites + 1)

# drop the full text (copyright reasons)
tweets <- subset( tweets, select = -c(tweet_cleaned, tweet_raw) )

# write dataset
fwrite( tweets, file = here('Congress Tweets', 'tweets_merged.csv') )