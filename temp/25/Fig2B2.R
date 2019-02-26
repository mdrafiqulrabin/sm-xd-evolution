# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(plyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(ggplot2)

# Methods
checkXD <- function(gs_id, fxi) {
  f = fgsfd %>% filter(fgsfd$google_id == gs_id)
  return(nrow(f) == 1 && f$XDIndicator == fxi)
}

# Read CSV file
fgsfd = read.csv("Data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
fgsfd = fgsfd %>% select(google_id, dept, XDIndicator)
gsps  = read.csv("Data/GoogleScholar_paper_stats.csv")
gsps  = gsps %>% select(year, coauthor_codes)

gspst = filter(gsps, gsps$year %in% c(2014))
coauthors = gspst$coauthor_codes
print(nrow(gspst))

lf_wd = 0; lf_xd = 0
tfp = 0

for (c in coauthors) {
  ca  = unlist(strsplit(c, ","))
  fca = ca [! ca %in% c(0:2)]
  
  n = length(fca)
  if (n <= 1) next
  
  for (i in (1):(n-1)) {
    for (j in (i+1):(n)) {
      tfp = tfp + 1
      
      if (checkXD(fca[i],"XD") || checkXD(fca[j],"XD")) {
        lf_xd = lf_xd + 1
      } else {
        lf_wd = lf_wd + 1
      }
    }
  }
  
}

tfp

lf_wd
lf_xd

lf_wd / lf_xd

