# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/TableS4S5/")

# Import library
library(readr, warn.conflicts=F)

# Load data
df_ps = read.csv("../GoogleScholar_paper_stats.csv") # paper stats
df_gs = read.csv("../Faculty_GoogleScholar_Funding_Data_N4190.csv") # google scholars
df = df_ps[1:100,] # working df

# Methods
f_remove_pollinators <- function (coauth) {
  coauth = unlist(strsplit(as.character(coauth), ",")) # unlist
  coauth = coauth [! coauth %in% c(0:2)] # Remove pollinators
  coauth = paste(as.character(coauth), collapse=",") # relist
  return(as.character(coauth))
}

f_get_fxd <- function (gsid) {
  xd = (df_gs %>% filter(google_id==gsid))$XDIndicator
  return(as.character(xd))
}

# Main
df$coauthor_codes = sapply(df$coauthor_codes, f_remove_pollinators)
df$FXD = sapply(df$google_id, f_get_XDIndicator)

# Write data
fn = paste0("Panel_Analysis.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(df, file = fn, row.names=F)
print("Done")

