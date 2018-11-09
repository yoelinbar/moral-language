# This is expect to run after the main analyses. It depends on the data structures created by that script.

### Overall DW-Nominate effects on moral language ###
### Effect of voting record on each MFD category, with random intercepts for authors ###

for (i in foundations)  {
  cat( paste( "\n############### Overall frequency for:", i, "(DW-Nominate) ###############\n") )
  dv <- as.matrix( byday.author[, i] )
  model <- lmer(dv ~ dim1 + (1+bs(totaldays)|author) + (1|totaldays), data=byday.author, REML=TRUE)
  print( summary(model) )
  # save model
  assign( paste(i, "mod.dwnom", sep = "."), model )
}

### EXTREMITY EFFECTS (DW-NOM W/IN PARTY) ###
for (i in foundations)  {
  cat( paste( "\n############### DW-Nominate effects for Democrats only:", i, "###############\n") )
  dv <- as.matrix( byday.author.D[, i] )
  model <- lmer(dv ~ dim1 + (1+bs(totaldays)|author) + (1|totaldays), data=byday.author.D, REML=TRUE)
  print( summary(model) )
  # save model
  assign( paste(i, "mod.dwnom.D", sep = "."), model )
}

for (i in foundations)  {
  cat( paste( "\n############### DW-Nominate effects for Republicans only:", i, "###############\n") )
  dv <- as.matrix( byday.author.R[, i] )
  model <- lmer(dv ~ dim1 + (1+bs(totaldays)|author) + (1|totaldays), data=byday.author.R, REML=TRUE)
  print( summary(model) )
  # save model
  assign( paste(i, "mod.dwnom.R", sep = "."), model )
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


## LIMIT TO FIRST THREE MONTHS OF 2016 ###
tweets_3_months = tweets[tweets$posixdate<as.POSIXct("2016-04-01"),]

for (i in foundations)  {
  cat( paste( "\n############### Overall frequency for:", i, " ###############\n") )
  dv <- as.matrix( tweets_3_months[, i] )
  model <- lmer(dv ~ partyd + (1|author) + (1|totaldays), data=tweets_3_months, REML=TRUE)
  print( summary(model) )
  # save model
  assign( paste(i, "mod.3mo", sep = "."), model )
  
}
