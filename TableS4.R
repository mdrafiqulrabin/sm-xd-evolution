# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(readr, warn.conflicts=F)

# Load data
df_ps = read.csv("Data/GoogleScholar_paper_stats.csv") # paper stats
df_gs = read.csv("Data/Faculty_GoogleScholar_Funding_Data_N4190.csv") # google scholars
df = df_ps # working df

# Methods
f_remove_pollinators <- function (coauth) {
  coauth = unlist(strsplit(as.character(coauth), ",")) # unlist
  coauth = coauth [! coauth %in% c(0:2)] # Remove pollinators
  coauth = paste(as.character(coauth), collapse=",") # relist
  return(coauth)
}

f_get_XDIndicator <- function (gsid) {
  xd = (df_gs %>% filter(google_id==gsid))$XDIndicator
  return(xd)
}

# Main
df$coauthor_codes = lapply(df_ps$coauthor_codes, f_remove_pollinators)
df$mxd = lapply(df_ps$google_id, f_get_XDIndicator)
df
