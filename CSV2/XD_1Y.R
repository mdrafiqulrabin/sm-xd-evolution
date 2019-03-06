# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV2/")

# Import library
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)

# Read CSV file
fId = read.csv("Id.csv")
n = nrow(fId)

# Create DataFrame
df <- data.frame(matrix(vector(), ncol=3))
colnames(df) <-c("Id", "google_id", "XDIndicator")

# Store to DataFrame
years = c(1970 : 2015) #"Data/GoogleScholar_paper_stats.csv"
for (y in years) {
  df <- df[c(), ] # Clear DataFrame
  csv_xd <- read.csv(paste0("XD_1Y/XD_",(y-1),".csv")) #previous xd file
  csv_st <- read.csv(paste0("UP_1Y/UP_",y,".csv")) #current source-target file
  for (i in (1):(n)) {
    f = fId[i,] # i-th fId
    xd = (csv_xd[i,])$XDIndicator # prev xd
    if (xd != "XD") {
      r_st = filter(csv_st, csv_st$Source==f$Id | csv_st$Target==f$Id)
      if (nrow(r_st) > 0 && length(which(r_st$s_dept != r_st$t_dept)) != 0) xd = "XD"
    }
    df <- rbind(df, data.frame(Id=f$Id, 
                google_id=f$google_id, XDIndicator=xd))
  }
  
  # Save to CSV
  fn = paste0("XD_1Y/XD_",y,".csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(df, file = fn, row.names=F)
  print(paste0("Done: XD -> ",y))
}