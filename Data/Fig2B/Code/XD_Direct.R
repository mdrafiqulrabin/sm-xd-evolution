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
df <- data.frame(matrix(vector(),ncol=3))
colnames(df) <-c("FacultyI","FacultyJ","XDIndicator")

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
    f_gid = c_gid [! c_gid %in% c(0:2)] # Remove pollinators
    f_gid = f_gid [f_gid != m_gid] # Remove left gid
    
    n = length(f_gid)
    
    if (n < 1) { # no faculty coauthor
      # TODO
      next
    } else { # has faculty coauthor
      for (i in (1):(n)) {
        tc = tc + 1
        f_xd = getDept(f_gid[i])
        m_xd = getDept(m_gid)
        if (m_xd == "BIO" && f_xd == "BIO") {
          xd_type = 0
        } else if (m_xd == "CS" && f_xd == "CS") {
          xd_type = 1
        } else {
          xd_type = 2
        }
        df <- rbind(df, data.frame(FacultyI=m_gid, FacultyJ=f_gid[i], XDIndicator=xd_type))
      }
    }
  }
  
  # Write CSV file
  fn = paste0("XD_Direct/",y,"_Direct.csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(df, file = fn, row.names=F)
  print(paste0("Done ", y))
}

tc #83171
