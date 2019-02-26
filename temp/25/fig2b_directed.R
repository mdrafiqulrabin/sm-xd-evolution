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
  return(nrow(f) == 1 && f$dept == xdi)
}

# Empty DataFrame
df <- data.frame(matrix(vector(),ncol=3))
colnames(df) <-c("FacultyI","FacultyJ","XDIndicator")

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
    
    n = length(fca)
    
    if (n <= 1) { # no/single faculty coauthor
      # TODO
      next
    } else { # multiple faculty coauthor
      for (i in (1):(n-1)) {
        for (j in (i+1):(n)) {
          tfp = tfp + 1
          xd_type = -1
          if (checkXD(fca[i],"BIO") && checkXD(fca[j],"BIO")) {
            xd_type = 0
          } else if (checkXD(fca[i],"CS") && checkXD(fca[j],"CS")) {
            xd_type = 1
          } else {
            xd_type = 2
          }
          df <- rbind(df, data.frame(FacultyI=fca[i], FacultyJ=fca[j], XDIndicator=xd_type))
        }
      }
    }
  }
#}
  
print ("Done")
tfp

lf_bi = length(which(df$XDIndicator == 0))
lf_cs = length(which(df$XDIndicator == 1))
lf_xd = length(which(df$XDIndicator == 2))

lf_xd / (lf_bi+lf_cs+lf_xd)  

