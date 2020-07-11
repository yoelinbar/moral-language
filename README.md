### OVERVIEW ###
These are replication scripts for the paper "Partisan Differences in Moral Language Use by U.S. Political Elites."
The data files are not included here due to GitHub's filesize limits. The scripts will attempt to download them from Dataverse.
Some smaller data files are included (see below).

If you would like to manually download the datasets:

Study 1: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SINBQH

Study 2: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/FQ8MIL

### REPLICATION SCRIPTS ###
These will run for anyone and can be used to reproduce the results in the paper.  

Study 1:  
Twitter/scripts/twitter_moral_language.R: Study 1 analyses  
Twitter/scripts/plots.R: generates figures for Twitter data (main and supplemental)  
Twitter/scripts/utils.R: some utility functions you probably don't need to worry about 

Study 2:  
Congress/scripts/congress_moral_language.R: Study 2 analyses  
Congress/scripts/plots.R: generates figures for Congress data

### INTERNAL-USE SCRIPTS ###
These scripts will not run unaltered. They are included for documentation and versioning. 
   
scripts/ddr_validation.py: Compute F1 scores for DDR loadings  
scripts/data_merge.R: Merge tweet and legislator data to create shareable data file  
scripts/rater_reliability.R: Inter-rater reliability for human coders  
scripts/topic_modeling.R: topic models (requires access to full tweets) 
scripts/negative_binary_regressions.do: Stata do-file to model retweets and likes  

### DATA ###
data/RetweetPlot: Predicted number of retweets for levels of moral language from Stata's "margins" command; used to generate figure in Supplemental Material  
data/follower_ideology.csv: average ideological placement of a sample of 500 followers for each Congressperson

### NOTES ###
The dataset does not include the full tweet text for copyright reasons. Text for any tweet can be retrieved from Twitter's
API using the tweet GUID.
