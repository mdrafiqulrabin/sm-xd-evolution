# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(plyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(ggplot2)

# Read CSV file
fgsfd = read.csv("Data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
gsps  = read.csv("Data/GoogleScholar_paper_stats.csv")

# Methods
checkXD <- function(fca) {
  for (gid in fca) {
    f = fgsfd %>% filter(google_id == gid)
    if (f$XDIndicator == "XD") {
      return(TRUE)
    }
  }
  return(FALSE)
}

getFacultyGsId <- function(gid, y) {
  fca = c()
  coauths = gsps %>% filter(google_id == gid, year == y)
  coauths = coauths$coauthor_codes
  for (ca in coauths) {
    yca = unlist(strsplit(ca, ","))
    yca = yca [! yca %in% c(0:2)]
    fca = c(fca, yca)
  }
  #fca = unique(fca[fca != gid])
  fca = fca[fca != gid]
  return (fca)
}

# Work from Here

gsids = fgsfd$google_id
years = c(min(gsps$year):max(gsps$year))
years = c(1993)

lf_wd = 0; lf_xd = 0

for (y in years) {
  for (gid in gsids) {
    fca = getFacultyGsId(gid, y)
    n = length(fca)
    if (n == 0) next
    if (checkXD(fca)) {
      lf_xd = lf_xd + n
    } else {
      lf_wd = lf_wd + n
    }
  }
}


lf_wd
lf_xd

lf_xd / (0.000001 + lf_wd + lf_xd)

lf_xd / (0.000001 + lf_xd + (1/lf_wd))

(1/lf_xd) / (0.000001 + (1/lf_xd) + (1/lf_wd))

