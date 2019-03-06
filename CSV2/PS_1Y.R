# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV2/")

# Import library
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)

# Read CSV file
df = read.csv("../Data/GoogleScholar_paper_stats.csv")
df = df %>% select(year, coauthor_codes)
years = c(min(df$year) : max(df$year))

tc=0 # Test 

for (y in years) {
  # Data.Frame of y
  df_y = filter(df, df$year == y)
  
  # Write CSV file
  fn = paste0("PS_1Y/PS_",y,".csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(df_y, file = fn, row.names=F)
  
  # Test 
  n = nrow(df_y)
  print(paste0("Done -> ", y, " = ", n))
  tc = tc + n
}

tc #424828