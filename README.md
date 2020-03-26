### OVERVIEW ###
These are replication scripts for the paper "Partisan Differences in Moral Language Use by U.S. Political Elites."
The main dataset is not included here due to GitHub's filesize limits. The script will attempt to download it from Dataverse.
Some smaller data files are included (see below)

### REPLICATION SCRIPTS ###
These will run for anyone and can be used to reproduce the results in the paper  
scripts/twitter_moral_language.R: analyses in the body of the paper  
scripts/supplemental.R: analyses in the Supplemental Material  
scripts/plots.R: generates all figures (main and supplemental)  
scripts/utils.R: some utility functions you probably don't need to worry about  

### INTERNAL-USE SCRIPTS ###
These scripts will not run unaltered. They are included for documentation and versioning.  
scripts/ddr_validation.py: Compute F1 scores for DDR loadings  
scripts/rater_reliability.R: Inter-rater reliability for human coders  
scripts/topic_modeling.R: topic models (requires access to full tweets)

### DATA ###
data/RetweetPlot: Predicted number of retweets for levels of moral language from Stata's "margins" command; used to generate Figure 3  
data/follower_ideology.csv: average ideological placement of a sample of 500 followers for each Congressperson

### NOTES ###
The dataset does not include the full tweet text for copyright reasons. Text for any tweet can be retrieved from Twitter's
API using the tweet GUID.
