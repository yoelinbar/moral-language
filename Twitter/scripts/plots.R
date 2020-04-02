# Separate data file for retweet plot (from Stata)
retweets <- read.csv( here('data', 'RetweetPlot.csv') )

### SCATTERPLOT BY SPEAKER ###
# Calculate speaker means
# Need to make sure we call the right version of summarize()
scatterplotdata <- tweets.long %>% group_by(author, foundation, Party) %>% dplyr::summarize(v_mean=(mean(loading)))

# Reorder columns and define matching labels for the plot
categoryorder <- c("Carevirtue", "Carevice", "Fairnessvirtue", "Fairnessvice", "Loyaltyvirtue", "Loyaltyvice", 
                      "Authorityvirtue", "Authorityvice", "Purityvirtue", "Purityvice")

xlabels <- c("Care-virtue", "Care-vice", "Fairness-virtue", "Fairness-vice", "Loyalty-virtue", "Loyalty-vice",
           "Authority-virtue", "Authority-vice", "Purity-virtue", "Purity-vice")

# this will be used by labeller() below
foundation_names <- c(
  `Carevirtue` = "Care-virtue",
  `Carevice` = "Care-vice",
  `Fairnessvirtue` = "Fairness-virtue",
  `Fairnessvice` = "Fairness-vice",
  `Loyaltyvirtue` = "Loyalty-virtue",
  `Loyaltyvice` = "Loyalty-vice",
  `Authorityvirtue` = "Authority-virtue",
  `Authorityvice` = "Authority-vice",
  `Purityvirtue` = "Purity-virtue",
  `Purityvice` = "Purity-vice"
)

scatterplotdata <- arrange(transform(scatterplotdata, foundation=factor(foundation,levels=categoryorder)), foundation)

set.seed(1)
scatterplotdata$numericfoundation <- as.numeric(scatterplotdata$foundation)
scatterplotdata$numericfoundation <- jitter(scatterplotdata$numericfoundation)

# Scatterplot with overlaid box
scatterplot <- ggplot(scatterplotdata, aes(x=foundation, y=v_mean, color=Party)) + 
  geom_jitter(aes(shape=Party, colour=Party)) + geom_boxplot(outlier.shape=NA, alpha=0.5)

print( scatterplot + theme_bw() +
         scale_colour_manual(values=c("royalblue4","#9E2E2E")) + 
         scale_x_discrete( labels = xlabels ) +
         labs(y="Mean Similarity") + labs(x="Moral Language Category") + 
         theme(
           text = element_text(size=20), 
           axis.text.x = element_text(angle=45, hjust=1), 
           panel.grid.major.x = element_blank()
          ) 
      )

### TIME SERIES PLOT ###
# Expect the main script to have run first, so just need to generate the prepost character variable
by_day$prepost_chr = ifelse(by_day$electiond == 0, "pre", "post")

# category ordering
by_day <- arrange(transform(by_day, foundation=factor(foundation,levels=categoryorder)), foundation)

print( ggplot(by_day, aes(x=totaldays, y=loading)) + geom_point(alpha = 0.5, shape = 16) + 
      geom_smooth(method="lm", aes(color=prepost_chr) ) + 
      theme_bw()  + facet_grid(Party ~ foundation, labeller = labeller(foundation = as_labeller(foundation_names) )) + 
      guides(color=FALSE) + labs(x = "Days since January 1, 2016", y = "Mean Similarity", color = "")
    )
  
### RETWEET PLOT ###
# Change category order
plotorder <- c("Care-virtue", "Fairness-virtue", "Authority-virtue", "Loyalty-virtue", "Purity-virtue", "Care-vice", "Fairness-vice", "Authority-vice", "Loyalty-vice", "Purity-vice")
retweets <- arrange(transform(retweets, Category=factor(Category,levels=plotorder)),Category)

retweetplot <- ggplot(retweets, aes(x=Similarity, y=Margin, geom=Affiliation))

print (retweetplot + geom_line(aes(colour=Affiliation)) + geom_point(aes(colour=Affiliation)) + facet_wrap(~Category, ncol=5, scales = "free") + 
         scale_colour_manual(labels=c("Democratic", "Republican"), values=c("royalblue4","#9E2E2E")) + 
         geom_ribbon(aes(ymin=CIlow, ymax=CIhigh), alpha=0.3) + ylab("Predicted Retweets") + labs(colour="Party") +
         scale_x_continuous(breaks=c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35)) + theme(axis.text.x = element_text(angle=90)) +
         theme_bw() + theme(legend.position = "top")
)

### DW-NOMINATE PLOT  (SUPPLEMENTAL) ###
dwnominateplot <- summarySEwithin(tweets.long, measurevar="loading", withinvars=c("election","foundation"), 
                                  betweenvars="dim1", idvar="author", na.rm=TRUE)
dwnominateplot <- arrange(transform(dwnominateplot, election=factor(election, levels=c("post","pre"))),election)
dwnominateplot <- arrange(transform(dwnominateplot, foundation=factor(foundation,levels=categoryorder)),foundation)

# summarySEwithin has converted this to a factor. To retrieve values, must call as.character() before as.numeric()
dwnominateplot$dim1 <- as.numeric(as.character(dwnominateplot$dim1))

# Plot
dwnominateplotted <- ggplot(dwnominateplot, aes(x=dim1, y=loading))

# Facet pre-post election
print( dwnominateplotted + geom_point(size=0.5, alpha=0.5, aes(colour=dim1)) + 
         scale_colour_gradient(low="royalblue4", high="#9E2E2E", breaks=c(-.746, .919), labels=c("Liberal", "Conservative")) +
         labs(y="Mean Loading") + ggtitle("Post- vs. Pre-Election") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
         geom_smooth(method="lm", se=TRUE, level=.95, alpha=.70, size=0.5, colour="gray11", fill="gray50") + 
         facet_grid(election~foundation) + theme(panel.spacing.y=unit(3, "lines")) +
         scale_x_continuous(name="DW-NOMINATE (Voting record)") + labs(colour="Ideology")
)

# Overall
print( dwnominateplotted + geom_point(size=0.5, alpha=0.5, aes(colour=dim1)) + 
         scale_colour_gradient(low="royalblue4", high="#9E2E2E", breaks=c(-.746, .919), labels=c("Liberal", "Conservative")) +
         labs(y="Mean Loading") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
         geom_smooth(method="lm", se=TRUE, level=.95, alpha=.70, size=0.5, colour="gray11", fill="gray50") + 
         facet_grid(~foundation) + theme(panel.spacing.y=unit(3, "lines")) +
         scale_x_continuous(name="DW-NOMINATE (Voting record)") + labs(colour="Ideology")
)

### FOLLOWER IDEOLOGY PLOT (SUPPLEMENTAL) ###
print( ggplot(follower.ideo, aes(x=Party, y=follower.ideo)) + geom_jitter(aes(colour=Party)) +
  geom_boxplot(outlier.shape=NA, alpha=0.5) + scale_colour_manual(values=c("royalblue4","#9E2E2E")) + 
  labs(y="More Conservative Ideology") + labs(x="Congressperson's Party") + ggtitle("Twitter Follower Ideology") +
  theme_bw() + theme(legend.position="none") )