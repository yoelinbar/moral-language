library(here)
library(data.table)
library(ggplot2)

# Compute average ideology of all members' followers and (optionally) plot results

follower.ideo <- fread( here('data', 'follower_ideology.csv'), data.table = FALSE  )

# Party dummy
follower.ideo$partyd <- as.numeric(follower.ideo$Party == "Democratic")

# Test the D-R difference
t.test(follower.ideo$follower.ideo[follower.ideo$Party=='Democratic'],
       follower.ideo$follower.ideo[follower.ideo$Party=='Republican'])

# Plot
ggplot(follower.ideo, aes(x=Party, y=follower.ideo)) + geom_jitter(aes(colour=Party)) +
  geom_boxplot(outlier.shape=NA, alpha=0.5) + scale_colour_manual(values=c("royalblue4","#9E2E2E")) + 
  labs(y="More Conservative Ideology") + labs(x="Congressperson's Party") + ggtitle("Twitter Follower Ideology") +
  theme_bw() + theme(legend.position="none")