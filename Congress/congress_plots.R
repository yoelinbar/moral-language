# This is expected to run after the main analyses. It depends on the data structures created by that script.

library(Rmisc)

### Long form data (used for some plots) ###

data.long <- melt(data, measure.var = c("Fairnessvirtue", "Carevirtue", 
                                        "Loyaltyvirtue", "Authorityvirtue", 
                                        "Purityvirtue", "Fairnessvice", 
                                        "Carevice", "Loyaltyvice", 
                                        "Authorityvice", "Purityvice"),
                  variable.name = "foundation",
                  value.name = "loading",
                  na.rm = TRUE
)

### SCATTERPLOT ###
bycongress.speaker.long <- melt(bycongress.speaker, measure.var = c("Fairnessvirtue", "Carevirtue", 
                                                                                  "Loyaltyvirtue", "Authorityvirtue", 
                                                                                  "Purityvirtue", "Fairnessvice", 
                                                                                  "Carevice", "Loyaltyvice", 
                                                                                  "Authorityvice", "Purityvice"),
                                       variable.name = "foundation",
                                       value.name = "loading",
                                       na.rm = TRUE
)

# Reorder columns and define matching labels for the plot
categoryorder <- c("Carevirtue", "Carevice", "Fairnessvirtue", "Fairnessvice", "Loyaltyvirtue", "Loyaltyvice", 
                   "Authorityvirtue", "Authorityvice", "Purityvirtue", "Purityvice")

xlabels <- c("Care-virtue", "Care-vice", "Fairness-virtue", "Fairness-vice", "Loyalty-virtue", "Loyalty-vice",
             "Authority-virtue", "Authority-vice", "Purity-virtue", "Purity-vice")

# this will be used by labeller() below
foundation_names <- c(
  `Carevirtue` = "Harm-virtue",
  `Carevice` = "Harm-vice",
  `Fairnessvirtue` = "Fairness-virtue",
  `Fairnessvice` = "Fairness-vice",
  `Loyaltyvirtue` = "Loyalty-virtue",
  `Loyaltyvice` = "Loyalty-vice",
  `Authorityvirtue` = "Authority-virtue",
  `Authorityvice` = "Authority-vice",
  `Purityvirtue` = "Purity-virtue",
  `Purityvice` = "Purity-vice"
)

scatterplotdata <- arrange(transform(bycongress.speaker.long, foundation=factor(foundation,levels=categoryorder)), foundation)

set.seed(1)
scatterplotdata$numericfoundation <- as.numeric(scatterplotdata$foundation)
scatterplotdata$numericfoundation <- jitter(scatterplotdata$numericfoundation)

scatterplot <- ggplot(scatterplotdata, aes(x=foundation, y=loading, color=party)) + 
  geom_jitter(aes(shape=party, colour=party), size = 1) + geom_boxplot(outlier.shape=NA, alpha=0.5)

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

### PARTY CONTROL PLOTS ###
data.long$chamberd <- ifelse(data.long$chamber == 'H', 0, 1)
plotdata <- summarySEwithin(data.long, measurevar="loading", betweenvars=c("partyd", "chamberd"), withinvars=c("congress", "foundation"), idvar="speaker")


## SENATE ##
plotdata_senate <- subset(plotdata, chamberd == 1)
plotdata_senate$congress <- as.numeric(levels(plotdata_senate$congress))[plotdata_senate$congress]

plot_senate <- ggplot(na.omit(plotdata_senate), aes(x=congress, y=loading, group=partyd))

plot_senate + facet_wrap(~foundation, nrow=2, ncol=5, labeller = labeller(foundation = as_labeller(foundation_names) ), scales = "free") +
  geom_rect(aes(xmin=114,
                xmax = Inf,
                ymin = -Inf,
                ymax = Inf), fill = 'firebrick2', alpha = 0.01) +
  geom_rect(aes(xmin=110,
                xmax = 114,
                ymin = -Inf,
                ymax = Inf), fill = 'lightskyblue1', alpha = 0.01) +
  geom_rect(aes(xmin=108,
                xmax = 110,
                ymin = -Inf,
                ymax = Inf), fill = 'firebrick2', alpha = 0.01) +
  geom_rect(aes(xmin=107,
                xmax = 108,
                ymin = -Inf,
                ymax = Inf), fill = 'lightskyblue1', alpha = 0.01) +
  geom_rect(aes(xmin=104,
                xmax = 107,
                ymin = -Inf,
                ymax = Inf), fill = 'firebrick2', alpha = 0.01) +
  geom_rect(aes(xmin=100,
                xmax = 104,
                ymin = -Inf,
                ymax = Inf), fill = 'lightskyblue1', alpha = 0.01) +
  geom_rect(aes(xmin=-Inf,
                xmax = 100,
                ymin = -Inf,
                ymax = Inf), fill = 'firebrick2', alpha = 0.01) +
  geom_point(size=0.4, aes(colour=partyd)) + geom_errorbar(aes(ymin=loading-se, ymax=loading+se, colour = partyd), width=.1) +
  geom_line(size=0.3, aes(colour=partyd)) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  scale_colour_manual(values=c("#9E2E2E", "royalblue4"), name="Party", labels = c("Republican", "Democratic")) +
  xlab("Congress") + ylab("Mean Similarity") + scale_x_continuous(breaks=c(98, 100, 102, 104, 106, 108, 110, 112, 114))


## HOUSE ##
plotdata_house <- subset(plotdata, chamberd == 0)
plotdata_house$congress <- as.numeric(levels(plotdata_house$congress))[plotdata_house$congress]


plot_house <- ggplot(na.omit(plotdata_house), aes(x=congress, y=loading, group=partyd))

plot_house + facet_wrap(~foundation, nrow=2, ncol=5, labeller = labeller(foundation = as_labeller(foundation_names) ), scales = "free") +
  geom_rect(aes(xmin=112,
                xmax = Inf,
                ymin = -Inf,
                ymax = Inf), fill = 'firebrick2', alpha = 0.01) +
  geom_rect(aes(xmin=110,
                xmax = 112,
                ymin = -Inf,
                ymax = Inf), fill = 'lightskyblue1', alpha = 0.01) +
  geom_rect(aes(xmin=104,
                xmax = 110,
                ymin = -Inf,
                ymax = Inf), fill = 'firebrick2', alpha = 0.01) +
  geom_rect(aes(xmin=-Inf,
                xmax = 104,
                ymin = -Inf,
                ymax = Inf), fill = 'lightskyblue1', alpha = 0.01) +
  geom_point(size=0.4, aes(colour=partyd)) + geom_errorbar(aes(ymin=loading-se, ymax=loading+se, colour = partyd), width=.1) +
  geom_line(size=0.3, aes(colour=partyd)) + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  scale_colour_manual(values=c("#9E2E2E", "royalblue4"), name="Party", labels = c("Republican", "Democratic")) +
  xlab("Congress") + ylab("Mean Similarity") + scale_x_continuous(breaks=c(98, 100, 102, 104, 106, 108, 110, 112, 114))
