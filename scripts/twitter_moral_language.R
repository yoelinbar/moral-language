### REQUIREMENTS ###
library(here)
library(data.table)

# Data processing
library(reshape2)
library(plyr)
library(dplyr)


# Modeling
library(lme4)
library(lmerTest)
library(nlme)
library(splines)

# Plotting
library(ggplot2)


# Utility functions
here <- here::here
source( here('Analysis Code', 'utils.R') )

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

### CREATE DATES AND ELECTION VARIABLE ###

# Write new column w/ dates in POSIX format
tweets$posixdate <- as.POSIXct(tweets$date, tz="GMT")

# 2016 Election Day: 11-08
tweets$election <- ifelse( tweets$posixdate<as.POSIXct("2016-11-09 00:00", tz="GMT"), 'pre', 'post' ) 

# limit to 2016 onwards
tweets <- tweets[tweets$posixdate >= as.POSIXct("2016-01-01 00:00"),]

# For time series analysis
# Days from beginning of sample
tweets$totaldays <- as.numeric( as.Date(tweets$posixdate) ) - as.numeric( as.Date( min(tweets$posixdate) ) )

# Dummy variable election
tweets$electiond <- as.numeric(tweets$election == "post")

# Days post the election, 0 if before the election
tweets$days_post_election <- ifelse( tweets$electiond==1, tweets$totaldays - 312, 0 )

### CLEANUP (VARIOUS) ###

# remove tweets from people when not in office (e.g. resigned or who took office in a special election)
# convert entry/exit dates to POSIX
tweets$posixdate.enter<-as.POSIXct(tweets$date.enter, tz="GMT")

# Just in case we have blanks here
tweets$date.exit[tweets$date.exit==''] <- NA
tweets$posixdate.exit<-as.POSIXct(tweets$date.exit, tz="GMT")

# Only tweets after taking office
tweets<-subset(tweets, posixdate>=posixdate.enter)
# We'll have NAs for people still in office
tweets<-subset(tweets, is.na(posixdate.exit) | posixdate<=posixdate.exit)

# Delete data from people with <50 tweets (doesn't play nice with MLM)
sub50 <- tweets %>% group_by(author) %>% filter(n() <50) %>% summarize()
tweets<- tweets[!(tweets$author %in% sub50$author),]

# NA for any foundation means DDR couldn't compute similiaries for that tweet
count_no_loading<-sum(is.na(tweets$Loyaltyvice))
tweets<-tweets[!is.na(tweets$Loyaltyvice),]

# Long format required for some analyses and figures 
tweets.long <- melt(tweets, measure.var = c("Fairnessvirtue", "Carevirtue", 
                                            "Loyaltyvirtue", "Authorityvirtue", 
                                            "Purityvirtue", "Fairnessvice", 
                                            "Carevice", "Loyaltyvice", 
                                            "Authorityvice", "Purityvice"),
                    variable.name = "foundation",
                    value.name = "loading",
                    na.rm = TRUE
)

# We'll be doing some separate analyses for Ds and Rs
tweets_D <- tweets[tweets$Party == "Democratic",]
tweets_R <- tweets[tweets$Party == "Republican",]

# Delete temporary data
rm( "author_metrics", "dwnominate", "dwnominate114", "dwnominate115", "loadings", "metrics","sub50" )

# Need to average over days in order to get these models to converge
byday.author <- tweets %>% group_by(totaldays, author) %>% summarize(
                                                                     Carevirtue = mean(Carevirtue),
                                                                     Carevice   = mean(Carevice),
                                                                     
                                                                     Fairnessvirtue = mean(Fairnessvirtue),
                                                                     Fairnessvice = mean(Fairnessvice),
                                                                     
                                                                     Loyaltyvirtue = mean(Loyaltyvirtue),
                                                                     Loyaltyvice = mean(Loyaltyvice),
                                                                     
                                                                     Authorityvirtue = mean(Authorityvirtue),
                                                                     Authorityvice = mean(Authorityvice),
                                                                     
                                                                     Purityvirtue = mean(Purityvirtue),
                                                                     Purityvice = mean(Purityvice),
                                                                     
                                                                     partyd = mean(partyd),
                                                                     dim1 = mean(dim1),
                                                                     electiond = mean(electiond)
                                                            )

byday.author.D <- byday.author[byday.author$partyd == 1,]
byday.author.R <- byday.author[byday.author$partyd == 0,]

#### 1. Effect of party by MFD category, with random intercepts for authors ####

foundations <- c( "Carevirtue", "Carevice", "Fairnessvirtue", "Fairnessvice", "Loyaltyvirtue", "Loyaltyvice", "Authorityvirtue",
                 "Authorityvice", "Purityvirtue",  "Purityvice" )

for (i in foundations)  {
  cat( paste( "\n############### Overall frequency for:", i, "###############\n") )
  dv <- as.matrix( byday.author[, i] )
  model <- lmer(dv ~ partyd + (1+bs(totaldays)|author) + (1|totaldays), data=byday.author, REML=TRUE)
  
  print( summary(model) )
  cat( paste("Effect size: d =", esize(model, 'partyd') ) )
  
  # save model
  assign( paste(i, "mod", sep = "."), model )
}

#### 2. Effect of time on each category separately for Ds and Rs, as well as tests of the interactions ####

for (i in foundations)  {
  cat( paste( "\n\n############### Time effects for:", i, "###############\n") )
  
  cat( '# Democrats:\n')
  dv <- as.matrix( byday.author.D[, i] )
  model <- lmer(dv ~ electiond + (1+bs(totaldays)|author) + (1|totaldays), data = byday.author.D, REML=TRUE)
  print( summary(model) )
  cat( paste("Democrat effect size: d =", esize(model, 'electiond') ) )
  # save model
  assign( paste(i, "modElectionD", sep = "."), model )
  
  cat( '\n\n# Republicans:\n')
  dv <- as.matrix( byday.author.R[, i] )
  model <- lmer(dv ~ electiond + (1+bs(totaldays)|author) + (1|totaldays), data = byday.author.R, REML=TRUE)
  print( summary(model) )
  cat( paste("Republican effect size: d =", esize(model, 'electiond') ) )
  # save model
  assign( paste(i, "modElectionR", sep = "."), model )

  cat( '\n\n# Interaction:\n')
  dv <- as.matrix( byday.author[, i] )
  model <- lmer(dv ~ partyd * electiond + (1+bs(totaldays)|author) + (1|totaldays), data=byday.author, REML=TRUE)
  print( summary(model) )
  # save model
  assign( paste(i, "modElectionInt", sep = "."), model )
}


#### 3. INTERRUPTED TIME SERIES ANALYSIS ####
# Summarize by day, foundation, and party
by_day <- tweets.long %>% group_by(totaldays, Party, foundation) %>% summarize(loading = mean(loading),
                                                                               electiond = mean(electiond),
                                                                               days_post_election = mean(days_post_election),
                                                                               partyd = mean(partyd)
)

by_day_R <- by_day[by_day$partyd == 0,]
by_day_D <- by_day[by_day$partyd == 1,]

for (i in foundations)  {
  cat( paste( "\n############### ITSA for:", i, "###############\n") )
  
  cat( '# Democrats:\n')
  data <- by_day_D[by_day_D$foundation == i,]
  model <- gls(loading ~ totaldays + days_post_election + electiond, correlation = corAR1(form = ~1), data=data)
  print( summary(model) )

  cat( '\n\n# Republicans:\n')
  data <- by_day_R[by_day_R$foundation == i,]
  model <- gls(loading ~ totaldays + days_post_election + electiond, correlation = corAR1(form=~1), data=data)
  print( summary(model) )
  
  cat( '\n\n# Interaction:\n')
  data <- by_day[by_day$foundation == i,]
  model <- gls(loading ~ totaldays + days_post_election + electiond*partyd, correlation = corAR1(form=~1), data=data)
  print( summary(model) )
}

#### 4. RETWEETS ####
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
}

#### PLOTS ####
source( here('Analysis Code', 'plots.R') )

