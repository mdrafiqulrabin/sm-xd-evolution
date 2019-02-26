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
    
    n = length(fca)
    m = length(pca)
    
    if (n == 0 ||  m == 0) { # no faculty/polinator coauthor
      # TODO
      next
    } else { # one/more polinator coauthor
      for (i in (1):(n)) {
        tfp = tfp + 1
        xd_type = -1
        if (checkXD(fca[i],"BIO")) {
          xd_type = 0
        } else if (checkXD(fca[i],"CS")) {
          xd_type = 1
        } else {
          xd_type = 2
        }
        df <- rbind(df, data.frame(Faculty=fca[i], XDIndicator=xd_type, NumOfPolinator=m))
      }
    }
  }
#}

print ("Done")
tfp

lp = aggregate(df$NumOfPolinator, by=list(df$XDIndicator), FUN=sum)
lp_bi = lp$x[1]
lp_cs = lp$x[2]
lp_xd = lp$x[3]

lp_xd / (lp_bi + lp_cs + lp_xd)  
