# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV/")

# Import library
library(dplyr, warn.conflicts = FALSE)
library(readr)

# Read CSV file
fgsfd = read.csv("../Data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
fgsfd = fgsfd %>% select(google_id, dept, XDIndicator)
n = nrow(fgsfd)

# Test
tc = 0

# Methods
setLabelToGoogleId <- function() {
  df <- data.frame(matrix(vector(), ncol=4))
  colnames(df) <-c('serial_id','google_id','dept','XDIndicator')
  for (i in (1):(n)) {
    f = fgsfd[i,]
    df <- rbind(df, data.frame(serial_id=i, google_id=f$google_id, dept=f$dept, XDIndicator=f$XDIndicator))
    tc = tc + 1
  }
  fn = paste0("Fig2A/Serial_Id.csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(df, file = fn, row.names = FALSE)
  print(paste0("Done: setLabelToGoogleId() : ",tc))
}

setLabelToGoogleId()
