# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV2/")

# Import library
library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)

# Read CSV file
csv_sid = read.csv("Fig2/Serial_ID.csv")
csv_paper = read.csv("../Data/GoogleScholar_paper_stats.csv")
csv_paper = csv_paper %>% select(year, coauthor_codes)

# Methods
getF <- function(gid) {
  f = csv_sid %>% filter(google_id == gid)
  return(f)
}

# Test
tc = 0

# DataFrame
df <- data.frame(matrix(vector(), ncol=5))
colnames(df) <-c("year","Source","s_dept","Target","t_dept")
df <- df[c(), ] # Clear DataFrame

# Main
years = c(min(csv_paper$year) : max(csv_paper$year))
for (y in years) {
  # [2] coauthor_codes
  allcoauthcode = filter(csv_paper, csv_paper$year == y)
  allcoauthcode = allcoauthcode[2] 
  
  n = nrow(allcoauthcode)
  for (i in (1):(n)) {
    f = allcoauthcode[i,]
    
    # Extract faculty gs_id
    f = unlist(strsplit(as.character(f), ","))
    f = f [! f %in% c(0:2)] # Remove pollinators
    
    # no faculty coauthor
    if (length(f) <= 1) next
    
    # pair of faculty
    p = t(combn(f, 2)) 
    for (i in (1):(nrow(p))) {
      tc = tc + 1
      s = getF(p[i,][1])
      t = getF(p[i,][2])
      df <- rbind(df, data.frame(year=y, Source=s$ID, s_dept=s$dept, Target=t$ID, t_dept=t$dept))
    }
  }
  print(paste0("Done ", y))
}

# Write CSV file
fn = paste0("Fig2/Source_Target.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(df, file = fn, row.names = FALSE)

tc #103568