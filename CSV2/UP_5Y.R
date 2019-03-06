# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV2/")

# Import library
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)

tc=0 # Test 

# DataFrame
df <- data.frame(matrix(vector(), ncol=2))
colnames(df) <-c("Source","Target")

years = seq(1970, 2015, by=5)
for (y in years) {
  df <- df[c(), ] # Clear DataFrame
  for (i in (0:4)) {
    fn = paste0("UP_1Y/UP_",(y-i),".csv")
    if (!file.exists(fn)) next
    df0 = read.csv(fn)
    df0 = df0 %>% select(Source, Target)
    df <- rbind(df, df0)
  }
  df = df[!duplicated(df), ]
  
  # Write CSV file
  fn = paste0("UP_5Y/UP_",y,".csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(df, file = fn, row.names=F)
  
  # Test 
  print(paste0("Done -> ", y, " = ", nrow(df)))
  tc = tc + nrow(df)
}
  
tc #22085