# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(readr, warn.conflicts=F)

# Load data
df_ps = read.csv("Data/GoogleScholar_paper_stats.csv")

# Methods
f_remove_pollinators <- function (x) {
  x = unlist(strsplit(as.character(x), ",")) # unlist
  x = x [! x %in% c(0:2)] # Remove pollinators
  x = paste(as.character(x), collapse=",") # relist
  return(x)
}

# Main
df_ps$coauthor_codes = lapply(df_ps$coauthor_codes, f_remove_pollinators)
df_ps
