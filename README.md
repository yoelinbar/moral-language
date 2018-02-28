### OVERVIEW ###
These are replication scripts and data for the paper "Partisan Differences in Moral Language Use by U.S. Political Elites."
All necessary data are included EXCEPT the DDR-computed dictionary similarities for each tweet. These are too large to store on github;
the R script will load them from http://dx.doi.org/10.7910/DVN/SINBQH

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

### NOTES ###
1. The main R script (twitter_moral_language.R) will load about 200 MB of data from the network. Don't run on a slow/metereed connection.
2. AllCongressTweets.csv does not contain the full tweet text for copyright reasons. Text for any tweet can be retrieved from Twitter's
API using the tweet GUID.
