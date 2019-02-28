# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV/")

# Import library
library(dplyr, warn.conflicts = FALSE)
library(readr)

# Read/Write CSV file for Edge
csv_sd= read.csv("Fig2A/Source_Target.csv")
n = nrow(csv_sd) #83171
t = 0
years = seq(1990, 2015, by=5)
for (y in years) {
  r_sd_5y = filter(csv_sd, csv_sd$year%in%c((y-4):y))
  r_sd_5y = r_sd_5y %>% select(Source, Target)
  t = t + nrow(r_sd_5y) # y[1986~2015]
  r_sd_5y = r_sd_5y[!duplicated(r_sd_5y), ] # Remove duplicate
  fn = paste0("Fig2A/",y,"_Edge.csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(r_sd_5y, file = fn, row.names = FALSE)
  print(paste0("Done: setEdge() : ",y))
}
