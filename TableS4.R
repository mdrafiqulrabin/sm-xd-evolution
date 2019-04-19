# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(readr, warn.conflicts=F)

# Load data
df_ps = read.csv("Data/GoogleScholar_paper_stats.csv")
df_ps
