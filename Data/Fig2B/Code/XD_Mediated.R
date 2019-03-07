# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/Fig2B/Data/")

# Import library
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)

# Read CSV file
fgsfd = read.csv("../../Faculty_GoogleScholar_Funding_Data_N4190.csv")
fgsfd = fgsfd %>% select(google_id, dept, XDIndicator)
gsps  = read.csv("../../GoogleScholar_paper_stats.csv")
gsps  = gsps %>% select(google_id, year, coauthor_codes)

# Methods
getDept <- function(gid) {
  f = fgsfd %>% filter(google_id == gid)
  return(as.character(f$dept))
}

# Empty DataFrame
df <- data.frame(matrix(vector(), ncol=3))
colnames(df) <-c("Faculty","XDIndicator","NumOfPolinator")

# Test
tc = 0

# Run
years = c(min(gsps$year) : max(gsps$year))
for (y in years) {
  df <- df[c(), ] # Clear DataFrame
  allcoauthcode = filter(gsps, gsps$year == y)
  for (i in (1):(nrow(allcoauthcode))) {
    eachcoauthcode = allcoauthcode[i,]
    m_gid = as.character(eachcoauthcode$google_id)
    # Extract faculty allcoauthcode
    c_gid = eachcoauthcode$coauthor_codes
    c_gid = unlist(strsplit(as.character(c_gid), ","))
    p_gid = c_gid [ c_gid %in% c(0:2) ] # Extract pollinators
    
    p0 = p_gid [ p_gid %in% c(0) ]
    p1 = p_gid [ p_gid %in% c(1) ]
    p2 = p_gid [ p_gid %in% c(2) ]
  
    n = length(p_gid)
    
    if (n < 1) { # no polinator coauthor
      # TODO
      next
    } else { # has polinator coauthor
      tc = tc + 1
      m_xd = getDept(m_gid)
      if (m_xd == "BIO") {
        if (length(p0) > 0) {
          df <- rbind(df, data.frame(Faculty=m_xd, XDIndicator=0, NumOfPolinator=length(p0)))
        }
      } else if (m_xd == "CS") {
        if (length(p1) > 0) {
          df <- rbind(df, data.frame(Faculty=m_xd, XDIndicator=1, NumOfPolinator=length(p1)))
        }
      } else {
        next
      }
      if (length(p2) > 0) {
        df <- rbind(df, data.frame(Faculty=m_xd, XDIndicator=2, NumOfPolinator=length(p2)))
      }
    }
  }
  
  # Write CSV file
  fn = paste0("XD_Mediated/",y,"_Mediated.csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(df, file = fn, row.names=F)
  print(paste0("Done ", y))
}

tc #389808
