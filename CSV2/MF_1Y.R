# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV2/")

# Import library
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)

tc=0 # Test 

# DataFrame 2
df_y2 <- data.frame(matrix(vector(), ncol=2))
colnames(df_y2) <-c("year","coauthor_codes")

years = c(1970 : 2015) #"Data/GoogleScholar_paper_stats.csv"
for (y in years) {
  # DataFrame 1
  fn = paste0("PS_1Y/PS_",y,".csv")
  df_y1 = read.csv(fn)
  df_y2 <- df_y2[c(), ] # Clear DataFrame
  
  n_y1 = nrow(df_y1)
  for (i in (1):(n_y1)) {
    cc = (df_y1[i,])$coauthor_codes
    cc = unlist(strsplit(as.character(cc), ",")) # unlist
    cc = cc [! cc %in% c(0:2)] # Remove pollinators
    if (length(cc)>1) {
      cc = paste(as.character(cc), collapse=",") # relist
      df_y2 <- rbind(df_y2, data.frame(year=y, coauthor_codes=cc))
    }
  }
  n_y2 = nrow(df_y2)
  
  # Write CSV file
  fn = paste0("MF_1Y/MF_",y,".csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(df_y2, file = fn, row.names=F)
  
  # Test 
  print(paste0("Done -> ", y, " = ", n_y1, " - ", n_y2))
  tc = tc + n_y2
}

tc #69014