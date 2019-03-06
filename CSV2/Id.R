# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV2/")

# Import library
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)

# Read CSV file
df_1 = read.csv("../Data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
df_1 = df_1 %>% select(google_id, dept, XDIndicator)
n = nrow(df_1)

# Test
tc = 0

# Methods
setLabelToGoogleId <- function() {
  df_2 <- data.frame(matrix(vector(), ncol=4))
  colnames(df_2) <-c('Id','google_id','dept','XDIndicator')
  for (i in (1):(n)) {
    f = df_1[i,]
    df_2 <- rbind(df_2, data.frame(Id=i, google_id=f$google_id, dept=f$dept, XDIndicator=f$XDIndicator))
    tc = tc + 1
  }
  fn = paste0("Id.csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(df_2, file = fn, row.names=F)
  print(paste0("Done: setLabelToGoogleId() : ",tc))
}

setLabelToGoogleId()
