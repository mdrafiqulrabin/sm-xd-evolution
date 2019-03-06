# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV2/")

# Import library
library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)

# Read/Write CSV file for Edge
csv_st= read.csv("Fig2/Source_Target.csv")
n = nrow(csv_st) #83171
tc = 0
years = seq(1990, 2015, by=5)
for (y in years) {
  r_st_5y = filter(csv_st, csv_st$year%in%c((y-4):y))
  r_st_5y = r_st_5y %>% select(Source, Target)
  tc = tc + nrow(r_st_5y) # y[1986~2015]
  r_st_5y = r_st_5y[!duplicated(r_st_5y), ] # Remove duplicate
  fn = paste0("Fig2/Edge_",y,".csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(r_st_5y, file = fn, row.names = FALSE)
  print(paste0("Done: Edge -> : ",y))
}

tc #103016