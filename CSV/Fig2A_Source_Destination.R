# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV/")

# Import library
library(dplyr, warn.conflicts = FALSE)
library(readr)

# Read CSV file
csv_sid = read.csv("Fig2A/Serial_Id.csv")
csv_gps = read.csv("../Data/GoogleScholar_paper_stats.csv")
csv_gps = csv_gps %>% select(google_id, year, coauthor_codes)
n = nrow(csv_gps)

# Test
tc = 0

# Methods
getSerialId <- function(gid) {
  f = csv_sid %>% filter(google_id == gid)
  return(as.character(f$serial_id))
}

getDept <- function(gid) {
  f = csv_sid %>% filter(google_id == gid)
  return(as.character(f$dept))
}

getXD <- function(gid) {
  f = csv_sid %>% filter(google_id == gid)
  return(as.character(f$XDIndicator))
}

# Main

# Create DataFrame
df <- data.frame(matrix(vector(), ncol=7))
colnames(df) <-c("year","source","s_dept","s_xd","destination","d_dept","d_xd")
df <- df[c(), ] # Clear DataFrame

# Store to DataFrame
for (i in (1):(n)) {
  p = csv_gps[i,] # p = publication
  s = p$google_id # s = source
  c = p$coauthor_codes # c = coauthors
  c = unlist(strsplit(as.character(c), ","))
  c = c [! c %in% c(0:2)] # Remove pollinators
  c = c [c != s] # Remove source gid
  if (length(c) < 1) next # No destination
  s_id = getSerialId(s)
  s_dp = getDept(s)
  s_xi = getXD(s)
  for (j in (1):(length(c))) {
    d = c[j] # d = destination
    d_id = getSerialId(d)
    d_dp = getDept(d)
    d_xi = getXD(d)
    df <- rbind(df, data.frame(year=p$year, 
                               source=s_id, s_dept=s_dp, s_xd=s_xi,
                               destination=d_id, d_dept=d_dp, d_xd=d_xi))
    tc = tc + 1
    print(tc)
  }
}

# Save to CSV
fn = paste0("Fig2A/Source_Destination.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(df, file = fn, row.names = FALSE)
print(paste0("Done: setSourceDestination() : ",tc)) #83171
