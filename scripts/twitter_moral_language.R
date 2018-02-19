### REQUIREMENTS ###
library(here)

# Data processing
library(reshape2)
library(plyr)
library(dplyr)
library(psych)

# Modeling
library(lme4)
library(lmerTest)
library(car)
library(miceadds)
library(multiwayvcov)
library(ggeffects)

# Plotting
library(ggrepel)
library(ggplot2)

# Utility functions
here <- here::here
source( here('Analysis Code', 'utils.R') )

### PREPROCESSING ###
tweets <- read.csv( here('Congress Tweets', 'AllCongressTweets.csv') )

# Merge DW-NOMINATE w/ tweet file
dwnominate115 <- read.csv( here('Congress Tweets', 'DWNOMINATE-115.csv') )
dwnominate114 <- read.csv( here('Congress Tweets', 'DWNOMINATE-114.csv') )

# Code which Congress(es) they were in
dwnominate115$C115 <- TRUE
dwnominate115$C114 <- ifelse( dwnominate115$twitter_handle %in% dwnominate114$twitter_handle, TRUE, FALSE )

# Need to do a little pre-processing on the 114th Congress. We drop those that are also in the 115th.
dwnominate114 <- dwnominate114[!dwnominate114$twitter_handle %in% dwnominate115$twitter_handle, ]

# Code which Congress(es) they were in
dwnominate114$C114 <- TRUE
dwnominate114$C115 <- FALSE

# We also need to recode party_code to D or R then drop that field
dwnominate114$affiliation <- ifelse(dwnominate114$party_code == 100, "D", "R")
dwnominate114<-within(dwnominate114, rm("party_code"))

# Merge Congresses and normalize twitter handle case
dwnominate<-rbind(dwnominate114, dwnominate115)
dwnominate$twitter_handle <- tolower(dwnominate$twitter_handle)

# Join on twitter handle
tweets <- merge(tweets,dwnominate, by.x="author", by.y = "twitter_handle", all.x=TRUE)

# Read in DDR output file
loadings <- read.table(file = here('DDR', 'document_dictionary_loadings.tsv'), sep = '\t', header = TRUE)
colnames(loadings) <- c("generated_id", "loyaltyVirtue", "authorityVice", "loyaltyVice", 
						"fairnessVice", "careVirtue", "authorityVirtue", "sanctityVice",
                        "sanctityVirtue", "careVice", "fairnessVirtue")
                        
# Merge
tweets <- merge(loadings,tweets,by="generated_id", all=TRUE)

# Tweet metrics (likes and RTs)
metrics <- read.csv(file = here('Congress Tweets', 'tweet_metrics.csv') )
tweets <- merge(tweets, metrics, by="generated_id", all.x=TRUE)

# Log- transform retweets/likes/followers
tweets$retweets_T <- log(tweets$retweets + 1)
tweets$favorites_T <- log(tweets$favorites + 1)
tweets$followers_T <- log(tweets$followers)

# Author metadata (followers, tweet count, account creation date)
author_metrics <- read.csv(file = here('Congress Tweets', 'author_metrics.csv') )
tweets <- merge(tweets, author_metrics, by="author", all.x=TRUE)

# NA for any foundation means DDR couldn't compute similiaries for that tweet
count_no_loading<-sum(is.na(tweets$loyaltyVice))
tweets<-tweets[!is.na(tweets$loyaltyVice),]

# Delete data from people with <50 tweets (doesn't play nice with MLM)
sub50 <- tweets %>% group_by(author) %>% filter(n() <50) %>% summarize()
tweets<- tweets[!(tweets$author %in% sub50$author),]

# Delete temporary data
rm( "author_metrics", "dwnominate", "dwnominate114", "dwnominate115", "loadings", "metrics","sub50" )

### CREATE ELECTION VARIABLE ###

# Write new columns w/ dates in POSIX format
tweets$posixdate <- as.POSIXct(tweets$date, tz="GMT")
tweets$created_at[tweets$created_at==""] <- NA # A few null strings from deleted accounts
tweets$posix_acctdate <-as.POSIXct(tweets$created_at, tz="GMT")

# 2016 Election Day: 11-08
tweets$election <- ifelse( tweets$posixdate<as.POSIXct("2016-11-09 00:00", tz="GMT"), 'pre', 'post' ) 

# limit to 2016 onwards
tweets <- tweets[tweets$posixdate >= as.POSIXct("2016-01-01 00:00"),]

# For time series analysis

# Days from beginning of sample
tweets$totaldays <- as.numeric( as.Date(tweets$posixdate) - as.Date( min(tweets$posixdate) ) )

# Dummy variable election
tweets$electiond <- as.numeric(tweets$election == "post")

# Days post the election, 0 if before the election
tweets$days_post_election <- ifelse( tweets$electiond==1, as.numeric( as.Date(tweets$posixdate) - as.Date("2017-11-08") ), 0 )


# Remove press accounts, RepJenniffer (most tweets in Spanish)
tweets <- tweets[!(tweets$author %in% c("SenMurphyOffice", "RepJenniffer", "InhofePress", "JimPressOffice", "MacTXPress", "McConnellPress", 
                                        "SenKaineOffice", "teammoulton")),]

# We have some Tweets from people in the 113th but not subsequent Congresses. Affiliation will be na for them.
tweets <- tweets[!is.na(tweets$affiliation),]

# We'll be doing some separate analyses for Ds and Rs
tweets_D <- tweets[tweets$affiliation == "D",]
tweets_R <- tweets[tweets$affiliation == "R",]

### 1. Effect of party by MFD category, with random intercepts for authors ###

foundations <- c("careVirtue", "careVice","fairnessVirtue","fairnessVice","loyaltyVirtue","loyaltyVice","authorityVirtue","authorityVice",
                 "sanctityVirtue", "sanctityVice" )

for (i in foundations)  {
  cat( paste( "\n############### Overall frequency for:", i, "###############\n") )
  dv <- tweets[, i]
  model <- lmer(dv ~ affiliation + (1|author), data=tweets, REML=TRUE)
  print( summary(model) )
  cat( paste("Effect size: d =", esize(model, 'affiliationR') ) )
}

### 2. Effect of time on each category separately for Ds and Rs, as well as tests of the interactions###

for (i in foundations)  {
  cat( paste( "\n\n############### Time effects for:", i, "###############\n") )
  
  cat( '# Democrats:\n')
  dv <- tweets_D[, i]
  model <- lmer(dv ~ election + (1|author), data=tweets_D, REML=TRUE)
  print( summary(model) )
  cat( paste("Democrat effect size: d =", esize(model, 'electionpre') ) )
  
  cat( '\n\n# Republicans:\n')
  dv <- tweets_R[, i]
  model <- lmer(dv ~ election + (1|author), data=tweets_R, REML=TRUE)
  print( summary(model) )
  cat( paste("Republican effect size: d =", esize(model, 'electionpre') ) )

  cat( '\n\n# Interaction:\n')
  dv <- tweets[, i]
  model <- lmer(dv ~ affiliation * election + (1|author), data=tweets, REML=TRUE)
  print( summary(model) )
}


### 3. INTERRUPTED TIME SERIES ANALYSIS ###
for (i in foundations)  {
  cat( paste( "\n############### ITSA for:", i, "###############\n") )
  
  cat( '# Democrats:\n')
  dv <- tweets_D[, i]
  model <- lm.cluster(data=tweets_D, formula = dv ~ totaldays + days_post_election + electiond, cluster = "author")
  summary(model)
  
  cat( '\n\n# Republicans:\n')
  dv <- tweets_R[, i]
  model <- lm.cluster(data=tweets_R, formula = dv ~ totaldays + days_post_election + electiond, cluster = "author")
  summary(model)
  
  cat( '\n\n# Interaction:\n')
  dv <- tweets[, i]
  model <- lm.cluster(data=tweets, formula = dv ~ totaldays + days_post_election + electiond * affiliation, cluster = "author")
  summary(model)
}

## 4. METRICS ##
# Retweets
for (i in foundations)  {
  cat( paste( "\n############### Retweets for:", i, "###############\n") )
  
  cat( '# Democrats:\n')
  iv <- tweets_D[, i]
  model <- lmer(retweets_T ~ iv + (1|author), data=tweets_D, REML=TRUE)
  print( summary(model) )
  
  cat( '\n\n# Republicans:\n')
  iv <- tweets_R[, i]
  model <- lmer(retweets_T ~ iv + (1|author), data=tweets_R, REML=TRUE)
  print( summary(model) )
  
  cat( '\n\n# Interaction:\n')
  iv <- tweets[, i]
  model <- lmer(retweets_T ~ iv * affiliation +  (1|author), data=tweets, REML=TRUE)
  print( summary(model) )
  # save for plotting
  assign( paste(i, "RT", sep=''), model)
}


# Likes
for (i in foundations)  {
  cat( paste( "\n############### Likes for:", i, "###############\n") )
  
  cat( '# Democrats:\n')
  iv <- tweets_D[, i]
  model <- lmer(favorites_T ~ iv + (1|author), data=tweets_D, REML=TRUE)
  print( summary(model) )
  
  cat( '\n\n# Republicans:\n')
  iv <- tweets_R[, i]
  model <- lmer(favorites_T ~ iv + (1|author), data=tweets_R, REML=TRUE)
  print( summary(model) )
  
  cat( '\n\n# Interaction:\n')
  iv <- tweets[, i]
  model <- lmer(favorites_T ~ iv * affiliation + (1|author), data=tweets, REML=TRUE)
  print( summary(model) )
}


### PLOTS ###
source( here('Analysis Code', 'plots.R') )

### EXPORT FOR STATA ###
write.table(tweets[, !names(tweets) %in% c("tweet_cleaned", "tweet_raw", "bioname")], 
            here('Congress Tweets', 'tweets-stata.csv'),
            na = "",
            row.names = FALSE,
            col.names = TRUE,
            append = FALSE,
            sep = ","
            )
