# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/Fig5/")

# Import library
library(readr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)

# Read CSV
df = read.csv("beta_i_all.csv")

# Histogram
hg = ggplot(data=df, aes(x=df$x, y=..count..)) +
  geom_histogram(binwidth = 0.006, colour = 'black', alpha=0.5)
hg
