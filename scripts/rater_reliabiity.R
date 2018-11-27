### Package loading ###
library('dplyr')
library('irr')
library('anchors')

### Load in dataframes ###
rater1 <- read.csv("rater1.csv")
rater2 <- read.csv("rater2.csv")
rater3 <- read.csv("rater3.csv")
rater4 <- read.csv("rater4.csv")

# Create list of categories to loop through
foundations <- c('HarmVirtue', 'HarmVice', 'FairnessVirtue', 'FairnessVice',
                 'LoyaltyVirtue', 'LoyaltyVice', 'AuthorityVirtue', 'AuthorityVice', 'SanctityVirtue',
                 'SanctityVice', 'None')

# Loop through foundations and print Fleiss Kappa values
for (i in foundations) {
  print(i)
  i <- as.data.frame(cbind(rater1[[i]], rater2[[i]], rater3[[i]], rater4[[i]]))
  
  
  colnames(i) <- c('rater1', 'rater2', 'rater3', 'rater4')
  print(kappam.fleiss(i))
}
