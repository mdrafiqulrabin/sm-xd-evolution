# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV/")

# Import library
library(dplyr, warn.conflicts = FALSE)
library(readr)

# Read CSV file
fgsfd = read.csv("../Data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
fgsfd = fgsfd %>% select(google_id, dept, XDIndicator)
gsps  = read.csv("../Data/GoogleScholar_paper_stats.csv")
gsps  = gsps %>% select(year, coauthor_codes)

# Methods
checkXD <- function(gid, xdi) {
  f = fgsfd %>% filter(google_id == gid)
  return(nrow(f) == 1 && f$dept == xdi)
}

# Empty DataFrame
df <- data.frame(matrix(vector(),ncol=3))
colnames(df) <-c("Faculty","XDIndicator","NumOfPolinator")

# Test
tfp = 0

# Run
years = c(min(gsps$year) : max(gsps$year))
for (y in years) {
  df <- df[c(), ] # Clear DataFrame
  coauthors = (filter(gsps, gsps$year %in% y))$coauthor_codes
  for (c in coauthors) {
    # Extract faculty coauthors
    ca  = unlist(strsplit(c, ","))
    fca = ca [! ca %in% c(0:2)]
    pca = ca [ ca %in% c(0:2) ]
    
    p0 = pca [ pca %in% c(0) ]
    p1 = pca [ pca %in% c(1) ]
    p2 = pca [ pca %in% c(2) ]
    
    n = length(fca)
    m = length(pca)
    
    if (n == 0 ||  m == 0) { # no faculty/polinator coauthor
      # TODO
      next
    } else { # one/more polinator coauthor
      for (i in (1):(n)) {
        tfp = tfp + 1
        if (checkXD(fca[i],"BIO")) {
          if (length(p0) > 0) {
            df <- rbind(df, data.frame(Faculty=fca[i], XDIndicator=0, NumOfPolinator=length(p0)))
          }
        } else if (checkXD(fca[i],"CS")) {
          if (length(p1) > 0) {
            df <- rbind(df, data.frame(Faculty=fca[i], XDIndicator=1, NumOfPolinator=length(p1)))
          }
        } else {
          next
        }
        if (length(p2) > 0) {
          df <- rbind(df, data.frame(Faculty=fca[i], XDIndicator=2, NumOfPolinator=length(p2)))
        }
      }
    }
  }
  fn = paste0("Fig2B_Mediated/",y,"_Mediated.csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(df, file = fn, row.names = FALSE)
  print(paste0("Done ", y))
}

tfp
