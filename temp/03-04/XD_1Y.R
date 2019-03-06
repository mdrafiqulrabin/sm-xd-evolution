# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV2/")

# Import library
library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)

# Read CSV file
csv_sid = read.csv("Fig2/Serial_ID.csv")
csv_st  = read.csv("Fig2/Source_Target.csv")
n = nrow(csv_sid)

# Create DataFrame
df <- data.frame(matrix(vector(), ncol=3))
colnames(df) <-c("ID", "google_id", "XDIndicator")

# Store to DataFrame
years = c(min(csv_st$year) : max(csv_st$year))
for (y in years) {
  df <- df[c(), ] # Clear DataFrame
  csv_py <- read.csv(paste0("Fig2/XD_",y-1,".csv")) # prev year csv
  for (i in (1):(n)) {
    f = csv_sid[i,] # i-th row of csv_sid
    xd = (csv_py[i,])$XDIndicator # prev year xd
    if (xd != "XD") {
      r_st  = filter(csv_st, csv_st$year==y,
                     csv_st$Source==f$ID | csv_st$Target==f$ID)
      r_st  = r_st[!duplicated(r_st), ] # Remove duplicate
      if (length(which(r_st$s_dept != r_st$t_dept)) != 0) xd = "XD"
    }
    df <- rbind(df, data.frame(ID=f$ID, 
                               google_id=f$google_id, XDIndicator=xd))
  }
  
  # Save to CSV
  fn = paste0("Fig2/XD_",y,".csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(df, file = fn, row.names = FALSE)
  print(paste0("Done: XD -> ",y))
}