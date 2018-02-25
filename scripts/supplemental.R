# This is expect to run after the main analyses. It depends on the data structures created by that script.

### Overall DW-Nominate effects on moral language ###
### Effect of voting record on each MFD category, with random intercepts for authors ###

for (i in foundations)  {
  cat( paste( "\n############### Overall frequency for:", i, "(DW-Nominate) ###############\n") )
  dv <- tweets[, i]
  model <- lmer(dv ~ dim1 + (1|author), data=tweets, REML=TRUE)
  print( summary(model) )

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

### EXTREMITY EFFECTS (DW-NOM W/IN PARTY) ###
for (i in foundations)  {
  cat( paste( "\n############### DW-Nominate effects for Democrats only:", i, "###############\n") )
  dv <- tweets_D[, i]
  model <- lmer(dv ~ dim1 + (1|author), data=tweets_D, REML=TRUE)
  print( summary(model) )
}

for (i in foundations)  {
  cat( paste( "\n############### DW-Nominate effects for Republicans only:", i, "###############\n") )
  dv <- tweets_R[, i]
  model <- lmer(dv ~ dim1 + (1|author), data=tweets_R, REML=TRUE)
  print( summary(model) )
}

## LIMIT TO FIRST THREE MONTHS OF 2016 ###
tweets_3_months = tweets[tweets$posixdate<as.POSIXct("2016-04-01"),]

for (i in foundations)  {
  cat( paste( "\n############### Overall frequency for:", i, "(DW-Nominate) ###############\n") )
  dv <- tweets_3_months[, i]
  model <- lmer(dv ~ partyd + (1|author), data=tweets_3_months, REML=TRUE)
  print( summary(model) )
  
}
