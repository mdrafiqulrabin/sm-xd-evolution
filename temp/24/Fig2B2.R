# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(plyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(ggplot2)

# Methods
checkFD <- function(gs_id, fxi) {
  f = fgsfd %>% filter(fgsfd$google_id == gs_id)
  return(nrow(f) == 1 && f$XDIndicator == fxi)
}

# Read CSV file
fgsfd = read.csv("data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
fgsfd = fgsfd %>% select(google_id, dept, XDIndicator)
gsps  = read.csv("Data/GoogleScholar_paper_stats.csv")
gsps  = gsps %>% select(year, coauthor_codes)

gspst = filter(gsps, gsps$year %in% c(1979,1980))
coauthors = gspst$coauthor_codes
print(nrow(gspst))

lf_cs = 0; lf_bio = 0; lf_xd = 0
lf_csxd = 0; lf_bioxd = 0
tfp = 0; lf_un = 0; lf_all = 0

for (c in coauthors) {
  ca  = unlist(strsplit(c, ","))
  fca = ca [! ca %in% c(0:2)]
  
  n = length(fca)
  if (n <= 1) next
  
  for (i in (1):(n-1)) {
    for (j in (i+1):(n)) {
      tfp = tfp + 1
      
      if (checkFD(fca[i],"CS") && checkFD(fca[j],"BIO")) {
        lf_xd = lf_xd + 1
      } else if (checkFD(fca[i],"BIO") && checkFD(fca[j],"CS")) {
        lf_xd = lf_xd + 1
      } else if (checkFD(fca[i],"CS") && checkFD(fca[j],"CS")) {
        lf_cs = lf_cs + 1
      } else if (checkFD(fca[i],"BIO") && checkFD(fca[j],"BIO")) {
        lf_bio = lf_bio + 1
      } else {
        lf_un = lf_un + 1
      }
    }
  }
  
}

lf_cs
lf_bio
lf_xd
lf_un

lf_all = lf_cs + lf_bio + lf_xd
lf_all

tfp

lf_xd/lf_all

