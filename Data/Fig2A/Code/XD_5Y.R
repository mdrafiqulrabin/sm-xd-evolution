# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/Fig2A/Data/")

# Import library
library(plyr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)

# 1990 XD
df <- read.csv("XD_1Y/XD_1990.csv")

# 1995~2015 XD
years = seq(1995, 2015, by=5)
for (y in years) {
  fn = paste0("XD_1Y/XD_",y,".csv")
  df0 = read.csv(fn)
  df <- cbind(df, XDIndicator=df0$XDIndicator)
}

# Column Rename
colnames(df) <- c("Id", "Label", 
                 "XD1990", "XD1995", "XD2000", 
                 "XD2005", "XD2010", "XD2015")

# Common XD
fn = paste0("XD_5Y.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(df, file = fn, row.names=F)
print(paste0("Done: Common XD -> ", nrow(df)))