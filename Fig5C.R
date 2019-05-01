# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/Fig5/")

# Import library
library(readr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)

# Read CSV
df = read.csv("beta_i_all.csv")

# Histogram
hg = ggplot(data=df, aes(x=df$x, y=(..count..)/1000)) +
  geom_histogram(binwidth = 0.006, colour = 'black', alpha=0.5) +
  scale_x_continuous(breaks = seq(-0.05,0.20,0.05)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0.0,0.12,0.04)) +
  coord_cartesian(xlim = c(-0.11, 0.22), ylim = c(0.0, 0.15), clip = 'off') +
  xlab(expression(paste("Cross-disciplinary coefficient, ", beta[I], "", sep=""))) + 
  ylab(expression(paste("Prob. dist. ", P(beta[I]),"", sep=""))) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, size=0.5),
        plot.title = element_text(hjust = 0.5)) 
hg
