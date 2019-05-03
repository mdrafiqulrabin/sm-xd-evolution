# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Exploratory_Analysis/Data/FigS1/Data/")

# Import library
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)

tc=0 # Test 

# DataFrame
df <- data.frame(matrix(vector(), ncol=2))
colnames(df) <-c("Source","Target")
df <- df[c(), ] # Clear DataFrame

years = c(1970: 2015)
for (y in years) {
  fn = paste0("../../Fig2A/Data/UP_1Y/UP_",(y),".csv")
  if (!file.exists(fn)) next
  df0 = read.csv(fn)
  df0 = df0 %>% select(Source, Target)
  df <- rbind(df, df0)
}

df = df[!duplicated(df), ]

# Write CSV file
fn = paste0("UP_All.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(df, file = fn, row.names=F)

# Test 
print(paste0("Done -> ", nrow(df)))
tc = tc + nrow(df)
tc #15981