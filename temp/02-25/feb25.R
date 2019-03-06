# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(dplyr, warn.conflicts = FALSE)
library(readr)

# Read CSV file
fgsfd = read.csv("Data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
fgsfd = fgsfd %>% select(google_id, dept, XDIndicator)
gsps  = read.csv("Data/GoogleScholar_paper_stats.csv")
gsps  = gsps %>% select(year, coauthor_codes)

# Methods
checkXD <- function(gid, xdi) {
  f = fgsfd %>% filter(google_id == gid)
  return(nrow(f) == 1 && f$XDIndicator == xdi)
}

# Empty DataFrame
df <- data.frame(matrix(vector(),ncol=3))
colnames(df) <-c("Faculty","XDIndicator","NumOfPolinator")

# Test
tfp = 0

# Run
years = c(min(gsps$year) : max(gsps$year))
#for (y in years) {
y=c(1979,1980)
coauthors = (filter(gsps, gsps$year %in% y))$coauthor_codes
for (c in coauthors) {
  # Extract faculty coauthors
  ca  = unlist(strsplit(c, ","))
  fca = ca [! ca %in% c(0:2)]
  pca = ca [ ca %in% c(0:2) ]
  
  checkXD(fca[1],"BIO")
}

print ("Done")
tfp

#lp_bi = length(which(df$XDIndicator == 0))
#lp_cs = length(which(df$XDIndicator == 1))
#lp_xd = length(which(df$XDIndicator == 2))

#lf_xd / (lf_bi+lf_cs+lf_xd)  

#}
