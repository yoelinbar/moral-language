### OVERVIEW ###
These are replication scripts and data for the paper "Partisan Differences in Moral Language Use by U.S. Political Elites."
All necessary data are included.

### SCRIPTS ###
scripts/twitter_moral_language.R: analyses in the body of the paper  
scripts/supplemental.R: analyses in the Supplemental Material  
scripts/plots.R: generates all figures (main and supplemental)  
scripts/utils.R: some utility functions you probably don't need to worry about  

### DATA ###
data/AllCongressTweets.csv: tweets from all members of Congress, 2016-01-01 to 2018-01-30  
data/author_metrics.csv: metadata about each MoC; e.g. number of followers, dates they entered and left Congress, account creation date  
data/DWNOMINATE-114: DW-NOMINATE scores and metadata for each member of Congress 114  
data/DWNOMINATE-115: DW-NOMINATE scores and metadata for each member of Congress 115  
data/RetweetPlot: Predicted number of retweets for levels of moral language from Stata's "margins" command; used to generate Figure 3  
data/tweet_metrics.csv: number of retweets and favorites for each tweet  
data/follower_ideology.csv: average ideological placement of a sample of 500 followers for each Congressperson

### NOTES ###
AllCongressTweets.csv does not contain the full tweet text for copyright reasons. Text for any tweet can be retrieved from Twitter's
API using the tweet GUID.
